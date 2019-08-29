package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.core.meta.DefaultObjectMetaDataParser;
import org.hswebframework.ezorm.rdb.dialect.Dialect;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2TableMetaParser;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class DefaultRDBSchemaMetadataTest {

    public SyncSqlExecutor executor;

    private DefaultRDBSchemaMetadata schema;

    @Before
    public void init() {
        DefaultRDBDatabaseMetadata<DefaultRDBSchemaMetadata> database = new DefaultRDBDatabaseMetadata<>(Dialect.H2);

        schema = new DefaultRDBSchemaMetadata();
        schema.setName("PUBLIC");
        schema.setDatabase(database);
        executor = new TestSyncSqlExecutor(new H2ConnectionProvider());

        DefaultObjectMetaDataParser parser = new DefaultObjectMetaDataParser();
        parser.registerStrategy(new H2TableMetaParser(executor));
        schema.setParser(parser);

        database.addSchema(schema);
        database.setCurrentSchema(schema);

    }

    @Test
    public void test() {
        executor.execute(SqlRequests.of("CREATE TABLE IF NOT EXISTS test_table(" +
                "id varchar(32) primary key," +
                "name varchar(128) not null," +
                "age number(4)" +
                ")"));

        Assert.assertEquals(schema.getObject(RDBObjectType.table).size(), 1);


        RDBTableMetadata table = schema.getTable("test_table").orElseThrow(NullPointerException::new);

        Assert.assertEquals(table.getName(), "test_table");
        Assert.assertEquals(table.getAlias(), "test_table");

        Assert.assertTrue(table.getColumn("name").isPresent());
        Assert.assertTrue(table.getColumn("id").isPresent());


        Assert.assertTrue(table.findColumn("test_table.name").isPresent());

        Assert.assertTrue(table.findColumn("PUBLIC.test_table.name").isPresent());


    }


}