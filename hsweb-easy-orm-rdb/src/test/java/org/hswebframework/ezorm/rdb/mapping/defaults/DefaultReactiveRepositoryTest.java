package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.*;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.ForeignKeyBuilder;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.ddl.TableDDLResultOperator;
import org.hswebframework.ezorm.rdb.operator.dml.JoinType;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Date;
import java.util.List;

public class DefaultReactiveRepositoryTest {

    private ReactiveRepository<TestEntity, String> reactiveRepository;

    @Before
    public void init() {
        RDBDatabaseMetadata databaseMetadata = new RDBDatabaseMetadata(Dialect.H2);

        H2SchemaMetadata h2 = new H2SchemaMetadata("PUBLIC");
        databaseMetadata.setCurrentSchema(h2);
        databaseMetadata.addSchema(h2);
        databaseMetadata.addFeature(new TestSyncSqlExecutor(new H2ConnectionProvider()));
        databaseMetadata.addFeature(new TestReactiveSqlExecutor(new H2R2dbcConnectionProvider()));

        DefaultDatabaseOperator operator = DefaultDatabaseOperator.of(databaseMetadata);

        TableDDLResultOperator resultOperator = operator.ddl()
                .createOrAlter("entity_test")
                .addColumn("id").primaryKey().varchar(32).commit()
                .addColumn("name").varchar(32).commit()
                .addColumn("state").number(4).commit()
                .addColumn("create_time").alias("createTime").datetime().commit()
                .commit();

        operator.ddl()
                .createOrAlter("entity_detail")
                .addColumn("id").primaryKey().varchar(32).commit()
                .addColumn("name").varchar(32).commit()
                .commit()
                .reactive()
                .block();

        resultOperator.reactive().block();

        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(databaseMetadata);
        RDBTableMetadata table = parser
                .parseTable(TestEntity.class)
                .orElseThrow(NullPointerException::new);

        h2.addTable(table);

        table.addForeignKey(ForeignKeyBuilder.builder()
                .autoJoin(true)
                .toMany(false)
                .targetColumn("id")
                .sourceColumn("id")
                .joinType(JoinType.left)
                .target("entity_detail")
                .build());

        EntityResultWrapper<TestEntity> wrapper = new EntityResultWrapper<>(TestEntity::new);


        wrapper.setMapping(table.<EntityColumnMapping>getFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(TestEntity.class))
                .orElseThrow(NullPointerException::new));

        reactiveRepository = new DefaultReactiveRepository<>(
                DefaultDatabaseOperator.of(databaseMetadata), table, TestEntity.class, wrapper
        );

    }

    @Test
    public void testInsertBatchReactive() {
        List<TestEntity> list = Flux.range(0, 100)
                .map(i -> {
                    TestEntity entity = new TestEntity();
                    entity.setId("test_" + i);
                    entity.setName("test_" + i);
                    if (i < 10) {
                        entity.setState((byte) 1);
                    }
                    entity.setCreateTime(new Date());
                    return entity;
                })
                .collectList()
                .block();

        Assert.assertEquals(Integer.valueOf(100), reactiveRepository.insertBatch(Mono.just(list)).block());
    }


    @Test
    public void testCrudReactive() {
        TestEntity entity = new TestEntity();
        entity.setId("test");
        entity.setName("test");
        entity.setState((byte) 1);
        entity.setCreateTime(new Date());

        StepVerifier.create(reactiveRepository.insert(Mono.just(entity)))
                .expectNext(1)
                .verifyComplete();

        StepVerifier.create(reactiveRepository.findById(Mono.just(entity.getId())))
                .expectNext(entity)
                .verifyComplete();

        entity.setName("test2");

        StepVerifier.create(reactiveRepository.createUpdate()
                .set(entity::getName)
                .set(entity::getCreateTime)
                .where(entity::getId)
                .execute())
                .expectNext(1)
                .verifyComplete();

        entity.setName("test3");
        StepVerifier.create(reactiveRepository.save(Mono.just(entity))
                .map(SaveResult::getTotal))
                .expectNext(1)
                .verifyComplete();


        StepVerifier.create(reactiveRepository.deleteById(Mono.just(entity.getId())))
                .expectNext(1)
                .verifyComplete();


    }
}