package org.hswebframework.ezorm.rdb.metadata;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2TableMetadataParser;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class RDBSchemaMetadataTest {

    public SyncSqlExecutor executor;

    private RDBSchemaMetadata schema;

    @Before
    public void init() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);

        schema = new RDBSchemaMetadata("PUBLIC");
        schema.setDatabase(database);
        executor = new TestSyncSqlExecutor(new H2ConnectionProvider());

        schema.addFeature(executor);
        schema.addFeature(new H2TableMetadataParser(schema));

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