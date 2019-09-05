package org.hswebframework.ezorm.rdb.operator;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.core.meta.DefaultObjectMetaDataParser;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2TableMetaParser;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.Serializable;
import java.util.Map;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.of;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.singleMap;

public class DefaultDatabaseOperatorTest {

    RDBDatabaseMetadata database;

    DatabaseOperator operator;

    @Before
    public void init() {
        database = new RDBDatabaseMetadata(Dialect.H2);

        SyncSqlExecutor sqlExecutor = new TestSyncSqlExecutor(new H2ConnectionProvider());

        RDBSchemaMetadata schema = new RDBSchemaMetadata();
        schema.setName("PUBLIC");

        DefaultObjectMetaDataParser parser = new DefaultObjectMetaDataParser();
        parser.registerStrategy(new H2TableMetaParser(sqlExecutor));
        schema.setParser(parser);
        database.addSchema(schema);
        database.setCurrentSchema(schema);

        database.addFeature(sqlExecutor);
        operator = DefaultDatabaseOperator.of(database);

    }

    @Getter
    @Setter
    @AllArgsConstructor(staticName = "of")
    @NoArgsConstructor
    public static class TestEntity implements Serializable {
        private String id;
        private String name;

        private int status;
    }

    @Test
    public void testDmlCrud() {

        operator.sql()
                .sync()
                .execute(of("create table test_table(id varchar(32) primary key,name varchar(64) not null,status number(4) )"));

        TestEntity entity = TestEntity.of("test", "test_name", 1);

        TestEntity entity2= TestEntity.of("test2", "test2_name", 1);

        int len = operator
                .dml()
                .insert("test_table")
                .values(entity::getId, entity::getName, entity::getStatus)
                .values(entity2::getId, entity2::getName, entity2::getStatus)
                .execute()
                .sync();
        Assert.assertEquals(len, 2);

        len = operator
                .dml()
                .update("test_table")
                .set("name", "new_name")
                .where(dsl -> dsl.is("id", "test"))
                .execute()
                .sync();

        Assert.assertEquals(len, 1);


        Map<String, Object> data = operator.dml()
                .query()
                .select("id", "name")
                .from("test_table")
                .where(dsl -> dsl.is("id", "test"))
                .fetch(singleMap())
                .sync();
        Assert.assertEquals(data.get("id"), "test");
        Assert.assertEquals(data.get("name"), "new_name");

        len = operator.dml()
                .delete("test_table")
                .where(dsl -> dsl.is("id", "test"))
                .execute()
                .sync();

        Assert.assertEquals(len, 1);

    }
}