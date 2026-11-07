package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.core.CastUtil;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.events.ContextKeys;
import org.hswebframework.ezorm.rdb.events.EventContext;
import org.hswebframework.ezorm.rdb.events.EventListener;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.ReactiveCrudExecutionContext;
import org.hswebframework.ezorm.rdb.mapping.ReactiveCrudExecutionInterceptor;
import org.hswebframework.ezorm.rdb.mapping.TestEntity;
import org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.mapping.events.ReactiveResultHolder;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.h2.H2R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Supplier;

import static org.junit.Assert.*;

public class ReactiveVirtualUpdateLifecycleTest {

    private H2R2dbcConnectionProvider provider;
    private RDBTableMetadata table;
    private DefaultReactiveRepository<TestEntity, String> repository;
    private final AtomicInteger writes = new AtomicInteger();

    @Before
    public void setUp() {
        provider = new H2R2dbcConnectionProvider();
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        H2SchemaMetadata schema = new H2SchemaMetadata("PUBLIC");
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        database.addFeature(new TestReactiveSqlExecutor(provider) {
            @Override
            public Mono<Integer> update(Publisher<SqlRequest> request) {
                return super.update(Flux.from(request).doOnNext(sql -> writes.incrementAndGet()));
            }
        });
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
        TestEntity entity = new TestEntity();
        entity.setId("1");
        entity.setName("original");
        repository.insert(entity).block();
        writes.set(0);
    }

    @After
    public void close() {
        provider.getConnection().flatMap(connection -> Mono.from(connection.close())).block();
    }

    @Test
    public void virtualUpdateRetainsAwaitedLifecyclePayloadAndContextWithoutSqlWrites() {
        List<String> stages = new ArrayList<>();
        List<EventContext> events = new ArrayList<>();
        Map<String, Object> patch = Map.of("field", "new-value");
        intercept((execution, proceed) -> {
            execution.omitColumns("virtual");
            return execution.executeVirtualUpdate(() -> Mono.deferContextual(subscriber -> {
                assertEquals("request", subscriber.get("label"));
                stages.add("mutation");
                return repository.createQuery().setParam(execution.getQueryParam()).count();
            }));
        });
        listen(event -> {
            events.add(event);
            assertSame(repository, event.get(MappingContextKeys.repository).orElse(null));
            assertSame(table, event.get(ContextKeys.table).orElse(null));
            assertTrue(event.get(ContextKeys.source).orElse(null) instanceof DefaultReactiveUpdate);
            assertTrue(event.get(MappingContextKeys.reactive).orElse(false));
            assertFalse(event.get(MappingContextKeys.updateColumnInstance).orElseThrow(AssertionError::new)
                             .containsKey("virtual"));
            Map<String, Object> submitted = event.get(MappingContextKeys.submittedValues)
                                                 .orElseThrow(AssertionError::new);
            assertSame(patch, submitted.get("virtual"));
            try {
                submitted.put("other", "invalid");
                fail("submitted values must be immutable");
            } catch (UnsupportedOperationException expected) {
                // The event sees the original snapshot, not the mutable physical-column map.
            }
            ReactiveResultHolder holder = holder(event);
            holder.before(Mono.deferContextual(subscriber -> Mono.fromRunnable(() -> {
                assertEquals("request", subscriber.get("label"));
                stages.add("prepare");
            })));
            holder.invoke(Mono.fromRunnable(() -> stages.add("before")));
            holder.after(result -> Mono.fromRunnable(() -> stages.add("after:" + result)));
        });
        Mono<Integer> update = repository.createUpdate().set("virtual", patch).where("id", "1")
                                         .execute().contextWrite(context -> context.put("label", "request"));
        assertTrue(stages.isEmpty());
        assertEquals(Integer.valueOf(1), update.block());
        assertEquals(Integer.valueOf(1), update.block());
        assertEquals(Arrays.asList("prepare", "before", "mutation", "after:1",
                                   "prepare", "before", "mutation", "after:1"), stages);
        assertNotSame(events.get(0), events.get(1));
        assertNotSame(events.get(0).get(MappingContextKeys.submittedValues).orElse(null),
                      events.get(1).get(MappingContextKeys.submittedValues).orElse(null));
        assertEquals(0, writes.get());
        assertEquals("original", repository.findById("1").block().getName());
    }

    @Test
    public void nativeUpdateUsesSameLifecycleAndOriginalEntityPayload() {
        List<String> stages = new ArrayList<>();
        TestEntity input = new TestEntity();
        input.setName("changed");
        intercept((execution, proceed) -> proceed.get());
        listen(event -> {
            assertSame(input, event.get(MappingContextKeys.instance).orElse(null));
            assertEquals("changed", event.get(MappingContextKeys.submittedValues)
                                         .orElseThrow(AssertionError::new).get("name"));
            holder(event).before(Mono.fromRunnable(() -> stages.add("prepare")));
            holder(event).invoke(Mono.fromRunnable(() -> stages.add("before")));
            holder(event).after(result -> Mono.fromRunnable(() -> stages.add("after:" + result)));
        });
        assertEquals(Integer.valueOf(1), repository.createUpdate().set(input).where("id", "1").execute().block());
        assertEquals(Arrays.asList("prepare", "before", "after:1"), stages);
        assertEquals(1, writes.get());
        assertEquals("changed", repository.findById("1").block().getName());
    }

    @Test
    public void prepareAndBeforeFailuresPreventVirtualMutation() {
        IllegalStateException failure = new IllegalStateException("listener rejected");
        AtomicInteger mutated = new AtomicInteger();
        intercept((execution, proceed) -> execution.executeVirtualUpdate(() -> {
            mutated.incrementAndGet();
            return Mono.just(1);
        }));
        listen(event -> holder(event).before(Mono.error(failure)));
        StepVerifier.create(virtualUpdate()).expectErrorMatches(error -> error == failure).verify();
        listen(event -> holder(event).invoke(Mono.error(failure)));
        StepVerifier.create(virtualUpdate()).expectErrorMatches(error -> error == failure).verify();
        assertEquals(0, mutated.get());
        assertEquals(0, writes.get());
    }

    @Test
    public void mutationAndAfterFailuresReachCaller() {
        IllegalStateException mutationFailure = new IllegalStateException("mutation failed");
        IllegalStateException afterFailure = new IllegalStateException("after failed");
        AtomicInteger after = new AtomicInteger();
        intercept((execution, proceed) -> execution.executeVirtualUpdate(() -> Mono.error(mutationFailure)));
        listen(event -> holder(event).after(result -> Mono.fromRunnable(after::incrementAndGet)));
        StepVerifier.create(virtualUpdate()).expectErrorMatches(error -> error == mutationFailure).verify();
        assertEquals(0, after.get());
        intercept((execution, proceed) -> execution.executeVirtualUpdate(() -> Mono.just(1)));
        listen(event -> holder(event).after(result -> Mono.error(afterFailure)));
        StepVerifier.create(virtualUpdate()).expectErrorMatches(error -> error == afterFailure).verify();
        assertEquals(0, writes.get());
    }

    @Test
    public void cancellationCancelsMutationAndSkipsAfterListeners() {
        AtomicBoolean started = new AtomicBoolean();
        AtomicBoolean cancelled = new AtomicBoolean();
        AtomicBoolean after = new AtomicBoolean();
        intercept((execution, proceed) -> execution.executeVirtualUpdate(() -> {
            started.set(true);
            return Mono.<Integer>never().doOnCancel(() -> cancelled.set(true));
        }));
        listen(event -> holder(event).after(result -> Mono.fromRunnable(() -> after.set(true))));
        StepVerifier.create(virtualUpdate()).then(() -> assertTrue(started.get())).thenCancel().verify();
        assertTrue(cancelled.get());
        assertFalse(after.get());
        assertEquals(0, writes.get());
    }

    @Test
    public void repeatedOrMixedContinuationsRejectBeforeSecondMutation() {
        AtomicInteger virtualMutations = new AtomicInteger();
        intercept((execution, proceed) -> {
            Mono<Integer> mutation = execution.executeVirtualUpdate(() -> {
                virtualMutations.incrementAndGet();
                return Mono.just(1);
            });
            return mutation.then(mutation);
        });
        StepVerifier.create(virtualUpdate()).expectError(IllegalStateException.class).verify();
        assertEquals(1, virtualMutations.get());

        intercept((execution, proceed) -> proceed.get().then(execution.executeVirtualUpdate(() -> {
            virtualMutations.incrementAndGet();
            return Mono.just(1);
        })));
        StepVerifier.create(repository.createUpdate().set("name", "native").where("id", "1").execute())
                    .expectError(IllegalStateException.class).verify();
        assertEquals(1, virtualMutations.get());
        assertEquals(1, writes.get());
    }

    @Test
    public void virtualContinuationIsUnavailableForOtherOperations() {
        table.addFeature(new ReactiveCrudExecutionInterceptor() {
            @Override
            public <R> Mono<R> intercept(ReactiveCrudExecutionContext execution, Supplier<Mono<R>> proceed) {
                return execution.executeVirtualUpdate(() -> Mono.just(1)).map(CastUtil::<R>cast);
            }
        });
        StepVerifier.create(repository.createDelete().where("id", "1").execute())
                    .expectError(UnsupportedOperationException.class).verify();
        assertEquals(0, writes.get());
        assertNotNull(repository.findById("1").block());
    }

    private Mono<Integer> virtualUpdate() {
        return repository.createUpdate().set("virtual", "patch").where("id", "1").execute();
    }

    private void intercept(BiFunction<ReactiveCrudExecutionContext, Supplier<Mono<Integer>>, Mono<Integer>> flow) {
        table.addFeature(new ReactiveCrudExecutionInterceptor() {
            @Override
            public <R> Mono<R> intercept(ReactiveCrudExecutionContext execution, Supplier<Mono<R>> proceed) {
                return flow.apply(execution, () -> proceed.get().map(Integer.class::cast)).map(CastUtil::<R>cast);
            }
        });
    }

    private void listen(Consumer<EventContext> listener) {
        table.addFeature((EventListener) (type, event) -> {
            if (type == MappingEventTypes.update_before) {
                listener.accept(event);
            }
        });
    }

    private static ReactiveResultHolder holder(EventContext event) {
        return event.get(MappingContextKeys.reactiveResultHolder).orElseThrow(AssertionError::new);
    }
}
