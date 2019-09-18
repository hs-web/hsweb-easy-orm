package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.TestEntity;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.ddl.TableDDLResultOperator;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import reactor.core.publisher.Flux;

import java.util.Date;
import java.util.List;

public class DefaultSyncRepositoryTest {

    private SyncRepository<TestEntity, String> repository;


    @Before
    public void init() {
        RDBDatabaseMetadata databaseMetadata = new RDBDatabaseMetadata(Dialect.H2);

        H2SchemaMetadata h2 = new H2SchemaMetadata("PUBLIC");
        databaseMetadata.setCurrentSchema(h2);
        databaseMetadata.addSchema(h2);
        databaseMetadata.addFeature(new TestSyncSqlExecutor(new H2ConnectionProvider()));

        DefaultDatabaseOperator operator = DefaultDatabaseOperator.of(databaseMetadata);

        TableDDLResultOperator resultOperator = operator.ddl()
                .createOrAlter("entity_test")
                .addColumn("id").primaryKey().varchar(32).commit()
                .addColumn("name").varchar(32).commit()
                .addColumn("state").number(4).commit()
                .addColumn("create_time").alias("createTime").datetime().commit()
                .commit();

        resultOperator.sync();

        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(databaseMetadata);
        RDBTableMetadata table = parser.parseTable(TestEntity.class).orElseThrow(NullPointerException::new);

        h2.addTable(table);

        EntityResultWrapper<TestEntity> wrapper = new EntityResultWrapper<>(TestEntity::new);


        wrapper.setMapping(table.<EntityColumnMapping>getFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(TestEntity.class)).orElseThrow(NullPointerException::new));

        repository = new DefaultSyncRepository<>(
                DefaultDatabaseOperator.of(databaseMetadata), table, TestEntity.class, wrapper);

    }

    @Test
    public void testInsertBatch() {
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

        Assert.assertEquals(100, repository.insertBatch(list));
    }

    @Test
    public void testCrud() {
        TestEntity entity = new TestEntity();
        entity.setId("test");
        entity.setName("test");
        entity.setState((byte) 1);
        entity.setCreateTime(new Date());

        repository.insert(entity);

        TestEntity inDB = repository.findById("test").orElseThrow(NullPointerException::new);

        Assert.assertEquals(entity, inDB);
        entity.setName("test2");
        Assert.assertEquals(1, repository.createUpdate()
                .set(entity)
                .where(entity::getId)
                .execute());

        Assert.assertEquals(1, repository.createDelete()
                .where(entity::getId)
                .execute());

    }


}