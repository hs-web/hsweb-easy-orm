package org.hswebframework.ezorm.rdb.supports;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
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

import java.util.Date;
import java.util.List;

public abstract class BasicRepositoryTests {

    protected SyncRepository<BasicTestEntity, String> repository;

    protected abstract RDBSchemaMetadata getSchema();

    protected abstract Dialect getDialect();

    protected abstract SyncSqlExecutor getSqlExecutor();

    protected RDBDatabaseMetadata getDatabase() {
        RDBDatabaseMetadata metadata = new RDBDatabaseMetadata(getDialect());

        RDBSchemaMetadata schema = getSchema();

        metadata.setCurrentSchema(schema);
        metadata.addSchema(schema);
        metadata.addFeature(getSqlExecutor());

        return metadata;
    }

    @After
    public void after() {
        try {
            getSqlExecutor().execute(SqlRequests.of("drop table entity_test_table"));
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
                .sync();
        EntityResultWrapper<BasicTestEntity> wrapper = new EntityResultWrapper<>(BasicTestEntity::new);
        wrapper.setMapping(table.<EntityColumnMapping>getFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(BasicTestEntity.class)).orElseThrow(NullPointerException::new));


        repository = new DefaultSyncRepository<>(operator, table, BasicTestEntity.class, wrapper);
    }

    @Test
    public void testInsertBach() {
        List<BasicTestEntity> entities = Flux.range(0, 100)
                .map(integer -> BasicTestEntity.builder()
                        .id("test_id_" + integer)
                        .balance(1000L)
                        .name("test:" + integer)
                        .createTime(new Date())
                        .state((byte) 1)
                        .build())
                .collectList().block();
        Assert.assertEquals(100, repository.insertBatch(entities));
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

        repository.insert(entity);

        Assert.assertEquals(repository.findById("test_id").orElseThrow(NullPointerException::new), entity);


        List<BasicTestEntity> list = repository.createQuery()
                .where(entity::getId)
                .orderBy(SortOrder.desc("id"))
                .paging(0, 10)
                .fetch();

        Assert.assertEquals(list.get(0), entity);

        Assert.assertEquals(1, repository.createUpdate()
                .set(entity::getState)
                .where(entity::getId)
                .execute());

        Assert.assertEquals(1, repository.createDelete()
                .where(entity::getId)
                .execute());

    }

}
