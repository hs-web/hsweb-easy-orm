package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;
import java.sql.JDBCType;

import static org.hswebframework.ezorm.rdb.executor.SqlRequest.*;

public class MysqlTableMetaParserTest {

    private SyncSqlExecutor executor;

    private MysqlTableMetaParser parser;

    @Before
    public void init() {
        executor = new TestSyncSqlExecutor(new MysqlConnectionProvider());
        parser = new MysqlTableMetaParser(executor);
    }

    @Test
    public void testParse() {
        executor.execute(prepare("CREATE TABLE IF NOT EXISTS test_table(" +
                "id varchar(32) primary key," +
                "name varchar(128) not null," +
                "age int" +
                ")"));
        try {
            RDBTableMetaData metaData = parser.parse("test_table").orElseThrow(NullPointerException::new);

            //id
            {
                RDBColumnMetaData column = metaData.getColumn("id");

                Assert.assertNotNull(column);

                Assert.assertEquals(column.getDataType(), "varchar(32)");
                Assert.assertEquals(column.getJdbcType(), JDBCType.VARCHAR);
                Assert.assertEquals(column.getJavaType(), String.class);
                Assert.assertTrue(column.isNotNull());
                // 这里只解析表结构，而不会解析键信息.
                // Assert.assertTrue(column.isPrimaryKey());
            }

            //name
            {
                RDBColumnMetaData column = metaData.getColumn("name");

                Assert.assertNotNull(column);

                Assert.assertEquals(column.getDataType(), "varchar(128)");
                Assert.assertEquals(column.getLength(), 128);
                Assert.assertEquals(column.getJdbcType(), JDBCType.VARCHAR);
                Assert.assertEquals(column.getJavaType(), String.class);
                Assert.assertTrue(column.isNotNull());
            }

            //age
            {
                RDBColumnMetaData column = metaData.getColumn("age");

                Assert.assertNotNull(column);
                Assert.assertEquals(column.getPrecision(), 10);
                Assert.assertEquals(column.getScale(), 0);
                Assert.assertEquals(column.getDataType(), "int");
                Assert.assertEquals(column.getJdbcType(), JDBCType.INTEGER);
                Assert.assertEquals(column.getJavaType(), Integer.class);
            }
        } finally {
            executor.execute(prepare("drop table test_table;"));
        }

    }


}