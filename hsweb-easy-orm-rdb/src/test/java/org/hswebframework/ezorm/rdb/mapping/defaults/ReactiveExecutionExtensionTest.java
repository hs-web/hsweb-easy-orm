package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Sort;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.events.ContextKey;
import org.hswebframework.ezorm.rdb.events.EventListener;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.ReactiveCrudExecutionContext;
import org.hswebframework.ezorm.rdb.mapping.ReactiveCrudExecutionInterceptor;
import org.hswebframework.ezorm.rdb.mapping.ReactiveQueryResultContext;
import org.hswebframework.ezorm.rdb.mapping.ReactiveQueryResultTransformer;
import org.hswebframework.ezorm.rdb.mapping.ReactiveQuery;
import org.hswebframework.ezorm.rdb.mapping.ReactiveUpdate;
import org.hswebframework.ezorm.rdb.mapping.TestEntity;
import org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.SimpleFunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
import org.hswebframework.ezorm.rdb.supports.h2.H2R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import reactor.util.context.ContextView;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

public class ReactiveExecutionExtensionTest {

    private H2R2dbcConnectionProvider provider;
    private RDBTableMetadata table;
    private DefaultReactiveRepository<TestEntity, String> repository;

    @Before
    public void setUp() {
        provider = new H2R2dbcConnectionProvider();
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        H2SchemaMetadata schema = new H2SchemaMetadata("PUBLIC");
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        database.addFeature(new TestReactiveSqlExecutor(provider));
        DefaultDatabaseOperator operator = DefaultDatabaseOperator.of(database);
        operator.ddl().createOrAlter("entity_test")
                .addColumn("id").primaryKey().varchar(32).commit()
                .addColumn("name").varchar(64).commit()
                .addColumn("state").number(4).commit()
                .addColumn("create_time").alias("createTime").datetime().commit()
                .commit().reactive().block();
        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(database);
        table = parser.parseTableMetadata(TestEntity.class).orElseThrow(AssertionError::new);
        schema.addTable(table);
        EntityResultWrapper<TestEntity> wrapper = new EntityResultWrapper<>(TestEntity::new);
        wrapper.setMapping(table.getFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(TestEntity.class))
                                .map(EntityColumnMapping.class::cast).orElseThrow(AssertionError::new));
        repository = new DefaultReactiveRepository<>(operator, table, TestEntity.class, wrapper);
        repository.insert(Arrays.asList(entity("1", "one"), entity("2", "two"), entity("3", "three"))).block();
    }

    @After
    public void close() {
        provider.getConnection().flatMap(connection -> Mono.from(connection.close())).block();
    }

    @Test
    public void preparationAndHydrationShareIsolatedSubscriberState() {
        ContextKey<String> prepared = ContextKey.of("prepared");
        List<ReactiveQueryResultContext> executions = new ArrayList<>();
        table.addFeature((EventListener) (type, event) -> {
            if (type == MappingEventTypes.select_before) {
                ReactiveQueryResultContext execution = event.get(MappingContextKeys.reactiveQueryResultContext)
                                                           .orElseThrow(AssertionError::new);
                event.get(MappingContextKeys.reactiveResultHolder).orElseThrow(AssertionError::new)
                     .before(Mono.deferContextual(subscriber -> Mono.fromRunnable(() -> {
                         String label = subscriber.get("label");
                         execution.getAttributes().set(prepared, label);
                         execution.getQueryParam().context("prepared", label);
                     })));
            }
        });
        table.addFeature(new ReactiveQueryResultTransformer() {
            @Override
            public <T> Flux<T> transform(Flux<T> rows, ReactiveQueryResultContext context) {
                executions.add(context);
                String label = context.getAttributes().get(prepared).orElseThrow(AssertionError::new);
                assertEquals(label, context.getQueryParam().getContext("prepared").orElse(null));
                return rows.buffer(2).concatMap(batch -> Mono.deferContextual(subscriber -> {
                    assertEquals(label, subscriber.get("label"));
                    batch.forEach(row -> ((TestEntity) row).setName(label));
                    return Mono.just(batch);
                })).flatMapIterable(batch -> batch);
            }
        });

        QueryParam original = new QueryParam();
        original.setPaging(false);
        original.context("caller", "retained");
        Flux<TestEntity> query = repository.createQuery().setParam(original).orderBy(SortOrder.asc("id")).fetch();
        assertEquals(Arrays.asList("a", "a", "a"), names(query.contextWrite(context -> context.put("label", "a"))));
        assertEquals(Arrays.asList("b", "b", "b"), names(query.contextWrite(context -> context.put("label", "b"))));
        assertNotSame(executions.get(0), executions.get(1));
        assertNotSame(executions.get(0).getQueryParam(), executions.get(1).getQueryParam());
        assertFalse(original.getContext("prepared").isPresent());
        assertEquals("retained", original.getContext("caller").orElse(null));
    }

    @Test
    public void unregisteredTableRetainsExistingBeforeEventTiming() {
        AtomicInteger preparations = new AtomicInteger();
        table.addFeature((EventListener) (type, context) -> {
            if (type == MappingEventTypes.select_before) {
                preparations.incrementAndGet();
            }
        });
        Flux<TestEntity> rows = repository.createQuery().fetch();
        assertEquals(1, preparations.get());
        assertEquals(Long.valueOf(3), rows.count().block());
        assertEquals(Long.valueOf(3), rows.count().block());
        assertEquals(1, preparations.get());
    }

    @Test
    public void pagingAndFetchOneRetainOrderWhileCountsSkipTransformation() {
        AtomicInteger transformedRows = new AtomicInteger();
        List<String> operations = new ArrayList<>();
        table.addFeature(new ReactiveQueryResultTransformer() {
            @Override
            public <T> Flux<T> transform(Flux<T> rows, ReactiveQueryResultContext context) {
                operations.add(context.getOperation());
                return rows.doOnNext(row -> transformedRows.incrementAndGet());
            }
        });
        assertEquals(Arrays.asList("2"), repository.createQuery().orderBy(SortOrder.asc("id"))
            .copy().paging(1, 1).fetch().map(TestEntity::getId).collectList().block());
        // Explicit orders survive execution snapshots, and fetchOne limits native rows before enrichment.
        transformedRows.set(0);
        assertEquals("3", repository.createQuery().orderBy(SortOrder.desc("id")).fetchOne().block().getId());
        assertEquals(1, transformedRows.get());
        assertEquals("1", repository.createQuery().orderBy(SortOrder.asc("id")).paging(2, 1).fetchOne().block().getId());
        assertEquals(Integer.valueOf(3), repository.createQuery().count().block());
        assertEquals(Long.valueOf(3), repository.createQuery().countLong().block());
        assertEquals(Arrays.asList("fetch", "fetchOne", "fetchOne"), operations);
    }

    @Test
    public void dslSortPreparationUsesOneIsolatedListForEveryQueryOperation() {
        QueryParam original = new QueryParam();
        original.setPaging(false);
        original.getSorts().add(new Sort("name"));
        Map<String, Object> options = new HashMap<>();
        options.put("provider", "external");
        SortOrder virtualOrder = SortOrder.desc("virtualField").function("providerOrder")
                                          .value(42).options(options);
        List<ReactiveQueryResultContext> executions = new ArrayList<>();
        table.addFeature((EventListener) (type, event) -> {
            if (type == MappingEventTypes.select_before) {
                ReactiveQueryResultContext execution = event.get(MappingContextKeys.reactiveQueryResultContext)
                                                           .orElseThrow(AssertionError::new);
                executions.add(execution);
                List<Sort> sorts = execution.getQueryParam().getSorts();
                assertEquals(2, sorts.size());
                assertEquals("name", sorts.get(0).getName());
                Sort virtual = sorts.get(1);
                assertEquals("virtualField", virtual.getName());
                assertEquals("desc", virtual.getOrder());
                assertEquals("providerOrder", virtual.getType());
                assertEquals(42, virtual.getValue());
                assertEquals("external", virtual.getOpts().get("provider"));
                assertNotSame(options, virtual.getOpts());
                event.get(MappingContextKeys.reactiveResultHolder).orElseThrow(AssertionError::new)
                     .before(Mono.fromRunnable(() -> {
                         virtual.getOpts().put("provider", "prepared");
                         // Simulate a provider consuming a logical sort and choosing its SQL order.
                         sorts.clear();
                         sorts.add(new Sort("id").desc());
                     }));
            }
        });
        registerIdentityTransformer();
        ReactiveQuery<TestEntity> query = repository.createQuery().setParam(original)
                                                   .orderBy(virtualOrder).copy();
        Flux<TestEntity> rows = query.fetch();
        assertEquals(Arrays.asList("3", "2", "1"), rows.map(TestEntity::getId).collectList().block());
        assertEquals(Arrays.asList("3", "2", "1"), rows.map(TestEntity::getId).collectList().block());
        assertEquals("3", query.fetchOne().block().getId());
        assertEquals(Integer.valueOf(3), query.count().block());
        assertEquals(Long.valueOf(3), query.countLong().block());
        assertEquals(Arrays.asList("fetch", "fetch", "fetchOne", "count", "count"),
                     executions.stream().map(ReactiveQueryResultContext::getOperation).collect(Collectors.toList()));
        for (int index = 1; index < executions.size(); index++) {
            assertNotSame(executions.get(index - 1).getQueryParam(), executions.get(index).getQueryParam());
            assertNotSame(executions.get(index - 1).getQueryParam().getSorts(),
                          executions.get(index).getQueryParam().getSorts());
        }
        assertEquals(1, original.getSorts().size());
        assertEquals("name", original.getSorts().get(0).getName());
        assertEquals("external", options.get("provider"));
        assertEquals(1, query.getParam().getSorts().size());
    }

    @Test
    public void normalizedDslSortsPreserveNativeFunctionValueAndPrecedence() {
        table.addFeature(new SimpleFunctionFragmentBuilder("length", "String length"));
        QueryParam original = new QueryParam();
        original.setPaging(false);
        original.getSorts().add(new Sort("name").value("one"));
        ReactiveQuery<TestEntity> query = repository.createQuery().setParam(original)
            .orderBy(SortOrder.desc("name").function("length"), SortOrder.asc("id"));
        List<String> ordinary = query.fetch().map(TestEntity::getId).collectList().block();
        assertEquals(Arrays.asList("1", "3", "2"), ordinary);
        assertEquals(1, query.getParam().getSorts().size());
        registerIdentityTransformer();
        Flux<TestEntity> rows = query.copy().fetch();
        assertEquals(ordinary, rows.map(TestEntity::getId).collectList().block());
        assertEquals(ordinary, rows.map(TestEntity::getId).collectList().block());
        assertEquals(1, query.getParam().getSorts().size());
    }

    private void registerIdentityTransformer() {
        table.addFeature(new ReactiveQueryResultTransformer() {
            @Override
            public <T> Flux<T> transform(Flux<T> rows, ReactiveQueryResultContext context) {
                return rows;
            }
        });
    }

    @Test
    public void transformerErrorsAndCancellationReachCaller() {
        IllegalStateException failure = new IllegalStateException("enrichment failed");
        table.addFeature(new ReactiveQueryResultTransformer() {
            @Override
            public <T> Flux<T> transform(Flux<T> rows, ReactiveQueryResultContext context) {
                return Flux.error(failure);
            }
        });
        StepVerifier.create(repository.createQuery().fetch()).expectErrorMatches(error -> error == failure).verify();
        AtomicBoolean cancelled = new AtomicBoolean();
        table.addFeature(new ReactiveQueryResultTransformer() {
            @Override
            public <T> Flux<T> transform(Flux<T> rows, ReactiveQueryResultContext context) {
                return rows.concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true));
            }
        });
        StepVerifier.create(repository.createQuery().fetch(), 1).expectNextCount(1).thenCancel().verify();
        assertTrue(cancelled.get());
    }

    @Test
    public void crudSeesOriginalDuplicateInputBeforeMerging() {
        List<ReactiveCrudExecutionContext> executions = new ArrayList<>();
        AtomicInteger beforeNative = new AtomicInteger();
        table.addFeature(new ReactiveCrudExecutionInterceptor() {
            @Override
            public <R> Mono<R> intercept(ReactiveCrudExecutionContext context, Supplier<Mono<R>> proceed) {
                executions.add(context);
                List<?> original = (List<?>) context.getInput();
                assertEquals(2, original.size());
                assertEquals("kept", ((TestEntity) original.get(0)).getName());
                assertNull(((TestEntity) original.get(1)).getName());
                beforeNative.incrementAndGet();
                return proceed.get();
            }
        });
        TestEntity first = entity("duplicate", "kept");
        TestEntity second = entity("duplicate", null);
        second.setState((byte) 2);
        Mono<SaveResult> save = repository.save(Arrays.asList(first, second));
        assertEquals(0, beforeNative.get());
        assertEquals(1, save.block().getTotal());
        assertEquals(ReactiveCrudExecutionContext.Operation.SAVE, executions.get(0).getOperation());
        assertEquals("kept", repository.findById("duplicate").block().getName());
    }

    @Test
    public void savePublisherRetainsCallerContextWithoutInterceptor() {
        assertSaveContextBoundary(false);
    }

    @Test
    public void savePublisherSeparatesInputContextFromInterceptorAndLifecycleContext() {
        assertSaveContextBoundary(true);
    }

    private void assertSaveContextBoundary(boolean intercepted) {
        Logger callerLogger = LoggerFactory.getLogger("save-publisher-caller");
        Logger repositoryLogger = LoggerFactory.getLogger(TestEntity.class);
        assertNotSame(callerLogger, repositoryLogger);
        Object sentinelKey = new Object();
        Object sentinel = new Object();
        Object authentication = new Object();
        Object transaction = new Object();
        Object interceptorKey = new Object();
        Object interceptorValue = new Object();
        List<String> stages = new ArrayList<>();
        Consumer<ContextView> assertCallerKeys = context -> {
            assertSame(sentinel, context.get(sentinelKey));
            assertSame(authentication, context.get("authentication"));
            assertSame(transaction, context.get("transaction"));
        };
        Consumer<ContextView> assertRepositoryContext = context -> {
            assertCallerKeys.accept(context);
            assertSame(repositoryLogger, context.get(Logger.class));
        };
        if (intercepted) {
            table.addFeature(new ReactiveCrudExecutionInterceptor() {
                @Override
                public <R> Mono<R> intercept(ReactiveCrudExecutionContext execution, Supplier<Mono<R>> proceed) {
                    assertEquals(ReactiveCrudExecutionContext.Operation.SAVE, execution.getOperation());
                    return Mono.deferContextual(context -> {
                        assertRepositoryContext.accept(context);
                        assertFalse(context.hasKey(interceptorKey));
                        stages.add("interceptor");
                        return Mono.deferContextual(augmented -> {
                            assertRepositoryContext.accept(augmented);
                            assertSame(interceptorValue, augmented.get(interceptorKey));
                            stages.add("continuation");
                            return proceed.get();
                        }).contextWrite(current -> current.put(interceptorKey, interceptorValue));
                    });
                }
            });
        }
        table.addFeature((EventListener) (type, event) -> {
            if (type == MappingEventTypes.save_before) {
                Consumer<ContextView> assertLifecycleContext = context -> {
                    assertRepositoryContext.accept(context);
                    assertEquals(intercepted, context.hasKey(interceptorKey));
                    if (intercepted) {
                        assertSame(interceptorValue, context.get(interceptorKey));
                    }
                };
                event.get(MappingContextKeys.reactiveResultHolder).orElseThrow(AssertionError::new)
                     .before(Mono.deferContextual(context -> {
                         assertLifecycleContext.accept(context);
                         stages.add("before");
                         return Mono.empty();
                     }));
                event.get(MappingContextKeys.reactiveResultHolder).orElseThrow(AssertionError::new)
                     .invoke(Mono.deferContextual(context -> {
                         assertLifecycleContext.accept(context);
                         stages.add("invoke");
                         return Mono.empty();
                     }));
                event.get(MappingContextKeys.reactiveResultHolder).orElseThrow(AssertionError::new)
                     .after(result -> Mono.deferContextual(context -> {
                         assertLifecycleContext.accept(context);
                         assertEquals(1, ((SaveResult) result).getTotal());
                         stages.add("after");
                         return Mono.empty();
                     }));
            }
        });
        Mono<TestEntity> input = Mono.deferContextual(context -> {
            assertCallerKeys.accept(context);
            assertSame(callerLogger, context.get(Logger.class));
            assertFalse(context.hasKey(interceptorKey));
            stages.add("input");
            return Mono.just(entity("context-save", "persisted"));
        });
        Mono<SaveResult> save = repository.save(input)
            .flatMap(result -> Mono.deferContextual(context -> {
                assertCallerKeys.accept(context);
                assertSame(callerLogger, context.get(Logger.class));
                assertFalse(context.hasKey(interceptorKey));
                stages.add("result");
                return Mono.just(result);
            }))
            .contextWrite(context -> context.put(Logger.class, callerLogger)
                                            .put(sentinelKey, sentinel)
                                            .put("authentication", authentication)
                                            .put("transaction", transaction));
        assertTrue(stages.isEmpty());
        StepVerifier.create(save).assertNext(result -> assertEquals(1, result.getTotal())).verifyComplete();
        assertEquals(intercepted
                         ? Arrays.asList("input", "interceptor", "continuation", "before", "invoke", "after", "result")
                         : Arrays.asList("input", "before", "invoke", "after", "result"), stages);
        assertEquals("persisted", repository.findById("context-save").block().getName());
    }

    @Test
    public void mixedAndVirtualOnlyUpdatesRetainOriginalBuilderForResubscription() {
        List<ReactiveCrudExecutionContext> executions = new ArrayList<>();
        table.addFeature(new ReactiveCrudExecutionInterceptor() {
            @Override
            public <R> Mono<R> intercept(ReactiveCrudExecutionContext context, Supplier<Mono<R>> proceed) {
                assertEquals(ReactiveCrudExecutionContext.Operation.UPDATE, context.getOperation());
                assertEquals("opaque", context.getChangedValues().get("virtual"));
                executions.add(context);
                context.omitColumns("virtual");
                if (context.getChangedValues().size() == 1) {
                    return Mono.just(1).map(org.hswebframework.ezorm.core.CastUtil::<R>cast);
                }
                return proceed.get();
            }
        });
        ReactiveUpdate<TestEntity> virtual = repository.createUpdate().set("virtual", "opaque").where("id", "1");
        Mono<Integer> execute = virtual.execute();
        assertEquals(Integer.valueOf(1), execute.block());
        assertEquals(Integer.valueOf(1), execute.block());
        assertNotSame(executions.get(0), executions.get(1));
        assertNotSame(executions.get(0).getQueryParam(), executions.get(1).getQueryParam());
        assertEquals("one", repository.findById("1").block().getName());
        assertEquals(Integer.valueOf(1), repository.createUpdate().set("virtual", "opaque").set("name", "changed")
            .where("id", "1").execute().block());
        assertEquals("changed", repository.findById("1").block().getName());
    }

    @Test
    public void entityPayloadOmissionDoesNotMutateReusableUpdate() {
        AtomicInteger intercepted = new AtomicInteger();
        TestEntity input = entity(null, "not-persisted");
        table.addFeature(new ReactiveCrudExecutionInterceptor() {
            @Override
            public <R> Mono<R> intercept(ReactiveCrudExecutionContext context, Supplier<Mono<R>> proceed) {
                assertSame(input, context.getInput());
                assertEquals("not-persisted", context.getChangedValues().get("name"));
                context.omitColumns("name");
                intercepted.incrementAndGet();
                return proceed.get();
            }
        });
        input.setState((byte) 7);
        Mono<Integer> update = repository.createUpdate().set(input).where("id", "1").execute();
        assertEquals(Integer.valueOf(1), update.block());
        assertEquals(Integer.valueOf(1), update.block());
        assertEquals(2, intercepted.get());
        TestEntity persisted = repository.findById("1").block();
        assertEquals("one", persisted.getName());
        assertEquals(Byte.valueOf((byte) 7), persisted.getState());
        assertEquals("not-persisted", input.getName());
    }

    @Test
    public void deleteAndInsertInterceptOncePerNativeBatch() {
        List<ReactiveCrudExecutionContext.Operation> operations = new ArrayList<>();
        table.addFeature(new ReactiveCrudExecutionInterceptor() {
            @Override
            public <R> Mono<R> intercept(ReactiveCrudExecutionContext context, Supplier<Mono<R>> proceed) {
                operations.add(context.getOperation());
                assertNotNull(context.getColumnMapping());
                if (context.getOperation() == ReactiveCrudExecutionContext.Operation.DELETE) {
                    assertEquals(1, context.getQueryParam().getTerms().size());
                } else {
                    assertEquals(2, ((Collection<?>) context.getInput()).size());
                }
                return proceed.get();
            }
        });
        assertEquals(Integer.valueOf(2), repository.insert(Arrays.asList(entity("4", "four"), entity("5", "five"))).block());
        Mono<Integer> delete = repository.createDelete().where("id", "1").execute();
        assertEquals(Integer.valueOf(1), delete.block());
        assertEquals(Integer.valueOf(0), delete.block());
        assertEquals(Arrays.asList(ReactiveCrudExecutionContext.Operation.INSERT,
                                  ReactiveCrudExecutionContext.Operation.DELETE,
                                  ReactiveCrudExecutionContext.Operation.DELETE), operations);
    }

    private static TestEntity entity(String id, String name) {
        TestEntity entity = new TestEntity();
        entity.setId(id);
        entity.setName(name);
        return entity;
    }

    private static List<String> names(Flux<TestEntity> rows) {
        return rows.map(TestEntity::getName).collect(Collectors.toList()).block();
    }
}
