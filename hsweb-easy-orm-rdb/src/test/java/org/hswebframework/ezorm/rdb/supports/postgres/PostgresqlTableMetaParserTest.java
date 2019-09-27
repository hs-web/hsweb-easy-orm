package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.posgres.PostgresqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.posgres.PostgresqlTableMetadataParser;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.prepare;

public class PostgresqlTableMetaParserTest {

    private SyncSqlExecutor executor;

    private PostgresqlTableMetadataParser parser;

    @Before
    public void init() {
        executor = new TestSyncSqlExecutor(new PostgresqlConnectionProvider());
        PostgresqlSchemaMetadata schema =new PostgresqlSchemaMetadata("public");
        schema.addFeature(executor);
        parser = new PostgresqlTableMetadataParser(schema);
    }

    @Test
    public void testParse() {
        executor.execute(SqlRequests.of("CREATE TABLE IF NOT EXISTS test_table(" +
                "id varchar(32) primary key," +
                "name varchar(128) not null," +
                "age int4," +
                "json1 json," +
                "json2 jsonb" +
                ")"));
        try {
            RDBTableMetadata metaData = parser.parseByName("test_table").orElseThrow(NullPointerException::new);

            //id
            {
                RDBColumnMetadata column = metaData.getColumn("id").orElseThrow(NullPointerException::new);

                Assert.assertNotNull(column);

                Assert.assertEquals(column.getDataType(), "varchar(32)");
                Assert.assertEquals(column.getSqlType(), JDBCType.VARCHAR);
                Assert.assertEquals(column.getJavaType(), String.class);
                Assert.assertTrue(column.isNotNull());
                // 这里只解析表结构，而不会解析键信息.
                // Assert.assertTrue(column.isPrimaryKey());
            }

            //name
            {
                RDBColumnMetadata column = metaData.getColumn("name").orElseThrow(NullPointerException::new);

                Assert.assertNotNull(column);

                Assert.assertEquals(column.getDataType(), "varchar(128)");
                Assert.assertEquals(column.getLength(), 128);
                Assert.assertEquals(column.getSqlType(), JDBCType.VARCHAR);
                Assert.assertEquals(column.getJavaType(), String.class);
                Assert.assertTrue(column.isNotNull());
            }

            //age
            {
                RDBColumnMetadata column = metaData.getColumn("age").orElseThrow(NullPointerException::new);

                Assert.assertNotNull(column);
                Assert.assertEquals(column.getPrecision(), 32);
                Assert.assertEquals(column.getScale(), 0);
                Assert.assertEquals(column.getDataType(), "int4");
                Assert.assertEquals(column.getSqlType(), JDBCType.INTEGER);
                Assert.assertEquals(column.getJavaType(), Integer.class);
            }
            //json
            {
                RDBColumnMetadata column = metaData.getColumn("json1").orElseThrow(NullPointerException::new);

                Assert.assertNotNull(column);
                Assert.assertEquals(column.getDataType(), "json");
                Assert.assertEquals(column.getSqlType(), JDBCType.CLOB);
                Assert.assertEquals(column.getJavaType(), String.class);
            }
            //jsonb
            {
                RDBColumnMetadata column = metaData.getColumn("json2").orElseThrow(NullPointerException::new);

                Assert.assertNotNull(column);
                Assert.assertEquals(column.getDataType(), "jsonb");
                Assert.assertEquals(column.getType().getId(), "jsonb");
                Assert.assertEquals(column.getSqlType(), JDBCType.CLOB);
                Assert.assertEquals(column.getJavaType(), String.class);
            }
        } finally {
            executor.execute(prepare("drop table test_table"));
        }

    }


}