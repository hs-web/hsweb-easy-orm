package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Date;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.prepare;

public class MSSQLTableMetaParserTest {

    private SyncSqlExecutor executor;

    private SqlServer2012TableMetadataParser parser;

    @Before
    public void init() {
        executor = new TestSyncSqlExecutor(new MSSQLConnectionProvider());

        SqlServerSchemaMetadata sqlServerSchemaMetadata=new SqlServerSchemaMetadata("dbo");
        sqlServerSchemaMetadata.addFeature(executor);

        parser = new SqlServer2012TableMetadataParser(sqlServerSchemaMetadata);
    }

    @Test
    public void testParse() {
        executor.execute(SqlRequests.of("CREATE TABLE dbo.test_table(" +
                "id varchar(32) primary key," +
                "name varchar(128) not null," +
                "age int," +
                "create_time datetime2" +
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
                Assert.assertTrue(column.isPrimaryKey());
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
                Assert.assertEquals(column.getPrecision(), 10);
                Assert.assertEquals(column.getScale(), 0);
                Assert.assertEquals(column.getDataType(), "int");
                Assert.assertEquals(column.getSqlType(), JDBCType.INTEGER);
                Assert.assertEquals(column.getJavaType(), Integer.class);
            }

            //create_time
            {
                RDBColumnMetadata column = metaData.getColumn("create_time").orElseThrow(NullPointerException::new);

                Assert.assertNotNull(column);
                Assert.assertEquals(column.getDataType(), "datetime2");
                Assert.assertEquals(column.getSqlType(), JDBCType.TIMESTAMP);
                Assert.assertEquals(column.getJavaType(), Date.class);
            }
        } finally {
            executor.execute(prepare("drop table test_table;"));
        }

    }


}