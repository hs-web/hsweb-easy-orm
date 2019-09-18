package org.hswebframework.ezorm.rdb.supports;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
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
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Date;
import java.util.List;

import static org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder.*;

public abstract class BasicReactiveRepositoryTests {

    protected ReactiveRepository<BasicTestEntity, String> repository;

    protected abstract RDBSchemaMetadata getSchema();

    protected abstract Dialect getDialect();

    protected abstract ReactiveSqlExecutor getSqlExecutor();

    protected RDBDatabaseMetadata getDatabase() {
        RDBDatabaseMetadata metadata = new RDBDatabaseMetadata(getDialect());

        RDBSchemaMetadata schema = getSchema();

        metadata.setCurrentSchema(schema);
        metadata.addSchema(schema);
        ReactiveSqlExecutor sqlExecutor = getSqlExecutor();

        metadata.addFeature(sqlExecutor);
        metadata.addFeature(ReactiveSyncSqlExecutor.of(sqlExecutor));

        return metadata;
    }

    @After
    public void after() {
        try {
            getSqlExecutor().execute(Mono.just(SqlRequests.of("drop table entity_test_table"))).block();
        } catch (Exception e) {

        }
    }

    @Before
    public void init() {
        RDBDatabaseMetadata metadata = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(metadata);

        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(metadata);

        RDBTableMetadata table = parser.parseTable(BasicTestEntity.class).orElseThrow(NullPointerException::new);

        operator.ddl()
                .createOrAlter(table)
                .commit()
                .reactive()
                .block();
        EntityResultWrapper<BasicTestEntity> wrapper = new EntityResultWrapper<>(BasicTestEntity::new);
        wrapper.setMapping(table.<EntityColumnMapping>getFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(BasicTestEntity.class)).orElseThrow(NullPointerException::new));


        repository = new DefaultReactiveRepository<>(operator, table, BasicTestEntity.class, wrapper);
    }

    @Test
    public void testInsertBach() {

        StepVerifier.create(repository.insert(Flux.range(0, 10)
                .map(integer -> BasicTestEntity.builder()
                        .id("test_id_2_" + integer)
                        .balance(1000L)
                        .name("test2:" + integer)
                        .createTime(new Date())
                        .state((byte) 1)
                        .build())))
                .expectNext(10)
                .verifyComplete();

        StepVerifier
                .create(repository.insertBatch(Flux.range(0, 100)
                        .map(integer -> BasicTestEntity.builder()
                                .id("test_id_" + integer)
                                .balance(1000L)
                                .name("test:" + integer)
                                .createTime(new Date())
                                .state((byte) 1)
                                .build())
                        .collectList()))
                .expectNext(100)
                .verifyComplete();

    }



    @Test
    public void testCurd() {
        BasicTestEntity entity = BasicTestEntity.builder()
                .id("test_id")
                .balance(1000L)
                .name("test")
                .createTime(new Date())
                .state((byte) 1)
                .build();

        StepVerifier.create(repository.insert(Mono.just(entity)))
                .expectNext(1)
                .verifyComplete();

        StepVerifier.create(repository.findById(Mono.just(entity.getId())))
                .expectNext(entity)
                .verifyComplete();

        StepVerifier.create(repository.createQuery().count())
                .expectNext(1)
                .verifyComplete();

        StepVerifier.create(repository.createQuery()
                .where(entity::getId)
                .orderBy(desc(entity::getId))
                .fetch())
                .expectNext(entity)
                .verifyComplete();

        StepVerifier.create(repository.createUpdate()
                .set(entity::getState)
                .where(entity::getId)
                .execute())
                .expectNext(1)
                .verifyComplete();

        StepVerifier.create(repository.createDelete()
                .where(entity::getId)
                .execute())
                .expectNext(1)
                .verifyComplete();

    }

}
