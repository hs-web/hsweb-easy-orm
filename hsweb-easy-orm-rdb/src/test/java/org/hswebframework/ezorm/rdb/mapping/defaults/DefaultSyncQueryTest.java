package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.DefaultEntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.TestEntity;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import reactor.util.context.Context;

import java.util.Date;

public class DefaultSyncQueryTest {

    private DefaultDatabaseOperator operator;

    @Before
    public void init() {
        RDBDatabaseMetadata databaseMetadata = new RDBDatabaseMetadata(Dialect.H2);

        H2SchemaMetadata h2 = new H2SchemaMetadata("PUBLIC");
        databaseMetadata.setCurrentSchema(h2);
        databaseMetadata.addSchema(h2);
        databaseMetadata.addFeature(new TestSyncSqlExecutor(new H2ConnectionProvider()));

        operator = DefaultDatabaseOperator.of(databaseMetadata);

    }

    @Test
    public void test() {
        operator.ddl()
                .createOrAlter("entity_test")
                .addColumn("id").primaryKey().varchar(32).commit()
                .addColumn("name").varchar(32).commit()
                .addColumn("state").number(4).commit()
                .addColumn("create_time").alias("createTime").datetime().commit()
                .commit()
                .sync();

        Date date= new Date();
        operator.dml()
                .insert("entity_test")
                .columns("id", "name", "state", "createTime")
                .values("test", "1234", 1, date)
                .execute()
                .sync();


        TableOrViewMetadata metadata=operator.getMetadata().getTableOrView("entity_test").orElseThrow(NullPointerException::new);
        EntityResultWrapper<TestEntity> wrapper = new EntityResultWrapper<>(TestEntity::new);
        DefaultEntityColumnMapping mapping=new DefaultEntityColumnMapping( metadata, TestEntity.class);
        metadata.addFeature(mapping);

        mapping.addMapping("id","id");
        mapping.addMapping("name","name");
        mapping.addMapping("state","state");
        mapping.addMapping("create_time","createTime");

        wrapper.setMapping(mapping);

        DefaultSyncQuery<TestEntity> query = new DefaultSyncQuery<>(metadata, mapping, operator.dml(), wrapper, Context.empty());

        Assert.assertEquals(1,  query.count());

        TestEntity entity = query.where("id","test").fetchOne().orElseThrow(NullPointerException::new);
        Assert.assertEquals(entity.getId(),"test");
        Assert.assertEquals(entity.getName(),"1234");
        Assert.assertEquals(entity.getState(),(Object)(byte)1);
        Assert.assertEquals(entity.getCreateTime(),date);



    }
}