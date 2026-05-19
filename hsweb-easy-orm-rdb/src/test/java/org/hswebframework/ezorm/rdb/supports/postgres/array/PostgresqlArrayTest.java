package org.hswebframework.ezorm.rdb.supports.postgres.array;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.DefaultValueGenerator;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
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
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.UUID;

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
