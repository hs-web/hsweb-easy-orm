package org.hswebframework.ezorm.rdb.supports.postgres.array;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.DefaultValueGenerator;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.annotation.ColumnType;
import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultSyncRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlR2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlConnectionProvider;
import org.junit.Assert;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Slf4j
public class PostgresqlArrayTest {

    @Test
    public void testSyncArrayField() {
        RDBDatabaseMetadata database = getSyncDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);
        SyncSqlExecutor executor = getSyncSqlExecutor();
        try {
            SyncRepository<ArrayEntity, String> repository = createSyncRepository(database, operator);

            ArrayEntity entity = new ArrayEntity();
            entity.setId("arr-sync");
            entity.setTags(new Short[]{1, 2, 3});
            entity.setKeywords(new String[]{"person", "white shirt", "glasses"});

            repository.insert(entity);

            ArrayEntity loaded = repository.findById("arr-sync").orElseThrow(NullPointerException::new);
            Assert.assertArrayEquals(new Short[]{1, 2, 3}, loaded.getTags());
            Assert.assertArrayEquals(new String[]{"person", "white shirt", "glasses"}, loaded.getKeywords());

            ArrayEntity byTags = repository
                .createQuery()
                .where(ArrayEntity::getTags, new Short[]{1, 2, 3})
                .fetchOne()
                .orElseThrow(NullPointerException::new);
            Assert.assertEquals("arr-sync", byTags.getId());

            ArrayEntity byKeywords = repository
                .createQuery()
                .where(ArrayEntity::getKeywords, new String[]{"person", "white shirt", "glasses"})
                .fetchOne()
                .orElseThrow(NullPointerException::new);
            Assert.assertEquals("arr-sync", byKeywords.getId());
        } finally {
            try {
                executor.execute(SqlRequests.of("drop table test_pg_array_basic"));
            } catch (Exception ignore) {
            }
        }
    }

    @Test
    public void testSyncRepositoryCrud() {
        RDBDatabaseMetadata database = getSyncDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);
        SyncSqlExecutor executor = getSyncSqlExecutor();
        try {
            SyncRepository<ArrayEntity, String> repository = createSyncRepository(database, operator);

            SaveResult initial = repository.save(Arrays.asList(
                entity("sync-save-1", new Short[]{1, 2}, new String[]{"person", "white shirt"}),
                entity("sync-save-2", new Short[]{2, 3}, new String[]{"vehicle", "white car"})
            ));
            Assert.assertEquals(2, initial.getTotal());

            SaveResult updated = repository.save(Arrays.asList(
                entity("sync-save-1", new Short[]{7, 8}, new String[]{"person", "glasses"}),
                entity("sync-save-3", new Short[]{3, 4}, new String[]{"event", "night"})
            ));
            Assert.assertEquals(2, updated.getTotal());

            Assert.assertArrayEquals(
                new Short[]{7, 8},
                repository.findById("sync-save-1").orElseThrow(NullPointerException::new).getTags()
            );

            Assert.assertEquals(2, repository.insertBatch(Arrays.asList(
                entity("sync-batch-1", new Short[]{8, 9}, new String[]{"batch", "one"}),
                entity("sync-batch-2", new Short[]{9, 10}, new String[]{"batch", "two"})
            )));

            Assert.assertEquals(1, repository.updateById(
                "sync-save-2",
                entity("sync-save-2", new Short[]{5, 6}, new String[]{"vehicle", "updated"})
            ));

            Assert.assertArrayEquals(
                new String[]{"vehicle", "updated"},
                repository.findById("sync-save-2").orElseThrow(NullPointerException::new).getKeywords()
            );

            List<String> pagedIds = repository
                .createQuery()
                .where(ArrayEntity::getTags, new Short[]{8, 9})
                .fetch()
                .stream()
                .map(ArrayEntity::getId)
                .collect(Collectors.toList());
            Assert.assertEquals(List.of("sync-batch-1"), pagedIds);

            Assert.assertEquals(1, repository
                .createUpdate()
                .set(ArrayEntity::getKeywords, new String[]{"dsl", "updated"})
                .where(ArrayEntity::getId, "sync-batch-2")
                .execute());

            Assert.assertArrayEquals(
                new String[]{"dsl", "updated"},
                repository.findById("sync-batch-2").orElseThrow(NullPointerException::new).getKeywords()
            );

            Assert.assertEquals(1, repository
                .createDelete()
                .where(ArrayEntity::getId, "sync-batch-1")
                .execute());

            Assert.assertTrue(repository.findById("sync-batch-1").isEmpty());
            Assert.assertEquals(2, repository.deleteById(Arrays.asList("sync-save-1", "sync-save-3")));
            Assert.assertEquals(1, repository.deleteById("sync-save-2"));
            Assert.assertEquals(1, repository.deleteById("sync-batch-2"));
        } finally {
            try {
                executor.execute(SqlRequests.of("drop table test_pg_array_basic"));
            } catch (Exception ignore) {
            }
        }
    }

    @Test
    public void testSyncArrayTerms() {
        RDBDatabaseMetadata database = getSyncDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);
        SyncSqlExecutor executor = getSyncSqlExecutor();
        try {
            SyncRepository<ArrayEntity, String> repository = createSyncRepository(database, operator);
            repository.insertBatch(Arrays.asList(
                entity("term-sync-1", new Short[]{1, 2, 3}, new String[]{"person", "white shirt", "glasses"}),
                entity("term-sync-2", new Short[]{2, 4}, new String[]{"person", "black jacket"}),
                entity("term-sync-3", new Short[]{5}, new String[]{"vehicle", "white"})
            ));

            assertIds(repository.createQuery()
                                .contains(ArrayEntity::getTags, new Short[]{1, 2})
                                .fetch()
                                .stream()
                                .map(ArrayEntity::getId)
                                .collect(Collectors.toList()),
                      "term-sync-1");

            assertIds(repository.createQuery()
                                .contained(ArrayEntity::getTags, new Short[]{1, 2, 3, 4})
                                .fetch()
                                .stream()
                                .map(ArrayEntity::getId)
                                .collect(Collectors.toList()),
                      "term-sync-1",
                      "term-sync-2");

            assertIds(repository.createQuery()
                                .overlap(ArrayEntity::getTags, new Short[]{2, 5})
                                .fetch()
                                .stream()
                                .map(ArrayEntity::getId)
                                .collect(Collectors.toList()),
                      "term-sync-1",
                      "term-sync-2",
                      "term-sync-3");

            assertIds(repository.createQuery()
                                .notOverlap(ArrayEntity::getTags, new Short[]{5})
                                .fetch()
                                .stream()
                                .map(ArrayEntity::getId)
                                .collect(Collectors.toList()),
                      "term-sync-1",
                      "term-sync-2");

            assertIds(repository.createQuery()
                                .in(ArrayEntity::getTags, new Short[]{2, 5})
                                .fetch()
                                .stream()
                                .map(ArrayEntity::getId)
                                .collect(Collectors.toList()),
                      "term-sync-1",
                      "term-sync-2",
                      "term-sync-3");

            assertIds(repository.createQuery()
                                .and("tags", TermType.in + "$all", new Short[]{1, 2})
                                .fetch()
                                .stream()
                                .map(ArrayEntity::getId)
                                .collect(Collectors.toList()),
                      "term-sync-1");

            assertIds(repository.createQuery()
                                .contains(ArrayEntity::getKeywords, "white shirt")
                                .fetch()
                                .stream()
                                .map(ArrayEntity::getId)
                                .collect(Collectors.toList()),
                      "term-sync-1");
        } finally {
            try {
                executor.execute(SqlRequests.of("drop table test_pg_array_basic"));
            } catch (Exception ignore) {
            }
        }
    }

    @Test
    public void testReactiveArrayField() {
        RDBDatabaseMetadata database = getReactiveDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);
        ReactiveSqlExecutor executor = getReactiveSqlExecutor();
        try {
            ReactiveRepository<ArrayEntity, String> repository = createReactiveRepository(database, operator);

            ArrayEntity entity = new ArrayEntity();
            entity.setId("arr-reactive");
            entity.setTags(new Short[]{4, 5, 6});
            entity.setKeywords(new String[]{"vehicle", "white", "plate"});

            repository.insert(Mono.just(entity))
                      .as(StepVerifier::create)
                      .expectNext(1)
                      .verifyComplete();

            repository.findById(Mono.just("arr-reactive"))
                      .as(StepVerifier::create)
                      .assertNext(loaded -> {
                          Assert.assertArrayEquals(new Short[]{4, 5, 6}, loaded.getTags());
                          Assert.assertArrayEquals(new String[]{"vehicle", "white", "plate"}, loaded.getKeywords());
                      })
                      .verifyComplete();

            repository
                .createQuery()
                .where(ArrayEntity::getTags, new Short[]{4, 5, 6})
                .fetch()
                .as(StepVerifier::create)
                .assertNext(loaded -> Assert.assertEquals("arr-reactive", loaded.getId()))
                .verifyComplete();
        } finally {
            try {
                executor.execute(Mono.just(SqlRequests.of("drop table test_pg_array_basic"))).block();
            } catch (Exception ignore) {
            }
        }
    }

    @Test
    public void testReactiveRepositoryCrud() {
        RDBDatabaseMetadata database = getReactiveDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);
        ReactiveSqlExecutor executor = getReactiveSqlExecutor();
        try {
            ReactiveRepository<ArrayEntity, String> repository = createReactiveRepository(database, operator);

            repository.save(Arrays.asList(
                          entity("reactive-save-1", new Short[]{11, 12}, new String[]{"person", "hat"}),
                          entity("reactive-save-2", new Short[]{12, 13}, new String[]{"vehicle", "black"})
                      ))
                      .as(StepVerifier::create)
                      .assertNext(result -> Assert.assertEquals(2, result.getTotal()))
                      .verifyComplete();

            repository.save(Arrays.asList(
                          entity("reactive-save-1", new Short[]{21, 22}, new String[]{"person", "updated"}),
                          entity("reactive-save-3", new Short[]{13, 14}, new String[]{"event", "gate"})
                      ))
                      .as(StepVerifier::create)
                      .assertNext(result -> Assert.assertEquals(2, result.getTotal()))
                      .verifyComplete();

            repository.findById("reactive-save-1")
                      .as(StepVerifier::create)
                      .assertNext(entity -> Assert.assertArrayEquals(new Short[]{21, 22}, entity.getTags()))
                      .verifyComplete();

            repository.insertBatch(Arrays.asList(
                          entity("reactive-batch-1", new Short[]{31, 32}, new String[]{"batch", "reactive-1"}),
                          entity("reactive-batch-2", new Short[]{32, 33}, new String[]{"batch", "reactive-2"})
                      ))
                      .as(StepVerifier::create)
                      .expectNext(2)
                      .verifyComplete();

            repository.updateById("reactive-save-2",
                                  Mono.just(entity("reactive-save-2",
                                                   new Short[]{15, 16},
                                                   new String[]{"vehicle", "updated"})))
                      .as(StepVerifier::create)
                      .expectNext(1)
                      .verifyComplete();

            repository.findById("reactive-save-2")
                      .as(StepVerifier::create)
                      .assertNext(entity -> Assert.assertArrayEquals(
                          new String[]{"vehicle", "updated"},
                          entity.getKeywords()))
                      .verifyComplete();

            repository
                .createQuery()
                .where(ArrayEntity::getTags, new Short[]{31, 32})
                .fetch()
                .map(ArrayEntity::getId)
                .collectList()
                .as(StepVerifier::create)
                .assertNext(ids -> Assert.assertEquals(List.of("reactive-batch-1"), ids))
                .verifyComplete();

            repository.createUpdate()
                      .set(ArrayEntity::getKeywords, new String[]{"dsl", "reactive"})
                      .where(ArrayEntity::getId, "reactive-batch-2")
                      .execute()
                      .as(StepVerifier::create)
                      .expectNext(1)
                      .verifyComplete();

            repository.findById("reactive-batch-2")
                      .as(StepVerifier::create)
                      .assertNext(entity -> Assert.assertArrayEquals(
                          new String[]{"dsl", "reactive"},
                          entity.getKeywords()))
                      .verifyComplete();

            repository.createDelete()
                      .where(ArrayEntity::getId, "reactive-batch-1")
                      .execute()
                      .as(StepVerifier::create)
                      .expectNext(1)
                      .verifyComplete();

            repository.findById("reactive-batch-1")
                      .as(StepVerifier::create)
                      .verifyComplete();

            repository.deleteById(Flux.just("reactive-save-1", "reactive-save-3"))
                      .as(StepVerifier::create)
                      .expectNext(2)
                      .verifyComplete();

            repository.deleteById(Arrays.asList("reactive-save-2", "reactive-batch-2"))
                      .as(StepVerifier::create)
                      .expectNext(2)
                      .verifyComplete();
        } finally {
            try {
                executor.execute(Mono.just(SqlRequests.of("drop table test_pg_array_basic"))).block();
            } catch (Exception ignore) {
            }
        }
    }

    @Test
    public void testReactiveArrayTerms() {
        RDBDatabaseMetadata database = getReactiveDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);
        ReactiveSqlExecutor executor = getReactiveSqlExecutor();
        try {
            ReactiveRepository<ArrayEntity, String> repository = createReactiveRepository(database, operator);

            repository.insertBatch(Arrays.asList(
                          entity("term-reactive-1", new Short[]{1, 2, 3}, new String[]{"person", "white shirt", "glasses"}),
                          entity("term-reactive-2", new Short[]{2, 4}, new String[]{"person", "black jacket"}),
                          entity("term-reactive-3", new Short[]{5}, new String[]{"vehicle", "white"})
                      ))
                      .as(StepVerifier::create)
                      .expectNext(3)
                      .verifyComplete();

            repository.createQuery()
                      .contains(ArrayEntity::getTags, new Short[]{1, 2})
                      .fetch()
                      .map(ArrayEntity::getId)
                      .collectList()
                      .as(StepVerifier::create)
                      .assertNext(ids -> assertIds(ids, "term-reactive-1"))
                      .verifyComplete();

            repository.createQuery()
                      .overlap(ArrayEntity::getTags, new Short[]{2, 5})
                      .fetch()
                      .map(ArrayEntity::getId)
                      .collectList()
                      .as(StepVerifier::create)
                      .assertNext(ids -> assertIds(ids, "term-reactive-1", "term-reactive-2", "term-reactive-3"))
                      .verifyComplete();

            repository.createQuery()
                      .and("tags", TermType.in + "$all", new Short[]{1, 2})
                      .fetch()
                      .map(ArrayEntity::getId)
                      .collectList()
                      .as(StepVerifier::create)
                      .assertNext(ids -> assertIds(ids, "term-reactive-1"))
                      .verifyComplete();

            repository.createQuery()
                      .in(ArrayEntity::getTags, new Short[]{2, 5})
                      .fetch()
                      .map(ArrayEntity::getId)
                      .collectList()
                      .as(StepVerifier::create)
                      .assertNext(ids -> assertIds(ids, "term-reactive-1", "term-reactive-2", "term-reactive-3"))
                      .verifyComplete();

            repository.createQuery()
                      .contains(ArrayEntity::getKeywords, "white shirt")
                      .fetch()
                      .map(ArrayEntity::getId)
                      .collectList()
                      .as(StepVerifier::create)
                      .assertNext(ids -> assertIds(ids, "term-reactive-1"))
                      .verifyComplete();
        } finally {
            try {
                executor.execute(Mono.just(SqlRequests.of("drop table test_pg_array_basic"))).block();
            } catch (Exception ignore) {
            }
        }
    }

    private ArrayEntity entity(String id, Short[] tags, String[] keywords) {
        ArrayEntity entity = new ArrayEntity();
        entity.setId(id);
        entity.setTags(tags);
        entity.setKeywords(keywords);
        return entity;
    }

    private void assertIds(List<String> ids, String... expected) {
        List<String> actual = ids.stream().sorted().collect(Collectors.toList());
        List<String> expect = Arrays.stream(expected).sorted().collect(Collectors.toList());
        Assert.assertEquals(expect, actual);
    }

    private SyncRepository<ArrayEntity, String> createSyncRepository(RDBDatabaseMetadata database, DatabaseOperator operator) {
        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(database);
        RDBTableMetadata table = parser.parseTableMetadata(ArrayEntity.class).orElseThrow(NullPointerException::new);
        operator.ddl().createOrAlter(table).commit().sync();

        EntityResultWrapper<ArrayEntity> wrapper = new EntityResultWrapper<>(ArrayEntity::new);
        wrapper.setMapping(table
                               .<EntityColumnMapping>getFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(ArrayEntity.class))
                               .orElseThrow(NullPointerException::new));
        return new DefaultSyncRepository<>(operator, table, ArrayEntity.class, wrapper);
    }

    private ReactiveRepository<ArrayEntity, String> createReactiveRepository(RDBDatabaseMetadata database, DatabaseOperator operator) {
        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(database);
        RDBTableMetadata table = parser.parseTableMetadata(ArrayEntity.class).orElseThrow(NullPointerException::new);
        operator.ddl().createOrAlter(table).commit().reactive().block();

        EntityResultWrapper<ArrayEntity> wrapper = new EntityResultWrapper<>(ArrayEntity::new);
        wrapper.setMapping(table
                               .<EntityColumnMapping>getFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(ArrayEntity.class))
                               .orElseThrow(NullPointerException::new));
        return new DefaultReactiveRepository<>(operator, table, ArrayEntity.class, wrapper);
    }

    private RDBDatabaseMetadata getSyncDatabase() {
        RDBDatabaseMetadata metadata = new RDBDatabaseMetadata(Dialect.POSTGRES);
        RDBSchemaMetadata schema = getSchema();
        metadata.setCurrentSchema(schema);
        metadata.addSchema(schema);
        metadata.addFeature(getSyncSqlExecutor());
        return metadata;
    }

    private RDBDatabaseMetadata getReactiveDatabase() {
        RDBDatabaseMetadata metadata = new RDBDatabaseMetadata(Dialect.POSTGRES);
        RDBSchemaMetadata schema = getSchema();
        metadata.setCurrentSchema(schema);
        metadata.addSchema(schema);
        ReactiveSqlExecutor executor = getReactiveSqlExecutor();
        metadata.addFeature(executor);
        metadata.addFeature(ReactiveSyncSqlExecutor.of(executor));
        return metadata;
    }

    private RDBSchemaMetadata getSchema() {
        PostgresqlSchemaMetadata schema = new PostgresqlSchemaMetadata("public");
        schema.addFeature(new DefaultValueGenerator() {
            @Override
            public String getSortId() {
                return "uuid";
            }

            @Override
            public RuntimeDefaultValue generate(ObjectMetadata meta) {
                return () -> UUID.randomUUID().toString().replace("-", "");
            }

            @Override
            public String getName() {
                return "UUID";
            }
        });
        return schema;
    }

    private SyncSqlExecutor getSyncSqlExecutor() {
        return new TestSyncSqlExecutor(new PostgresqlConnectionProvider());
    }

    private ReactiveSqlExecutor getReactiveSqlExecutor() {
        return new TestReactiveSqlExecutor(new PostgresqlR2dbcConnectionProvider());
    }

    @Setter
    @Getter
    @Table(name = "test_pg_array_basic")
    public static class ArrayEntity {
        @Id
        @Column(length = 32)
        private String id;

        @Column(nullable = false)
        @ColumnType(typeId = "smallint[]")
        private Short[] tags;

        @Column(nullable = false)
        @ColumnType(typeId = "text[]")
        private String[] keywords;
    }
}
