package org.hswebframework.ezorm.rdb.supports.h2;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData;
import org.hswebframework.ezorm.rdb.render.Sql;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;
import java.sql.JDBCType;

import static org.hswebframework.ezorm.rdb.executor.SqlRequest.*;

public class H2TableMetaParserTest {

    private H2TableMetaParser parser;

    private SyncSqlExecutor executor;

    @Before
    @SneakyThrows
    public void init() {

        Class.forName("org.h2.Driver");
        executor = new TestSyncSqlExecutor(new H2ConnectionProvider());

        parser = new H2TableMetaParser(executor);
        parser.setSchemaName("PUBLIC");
    }

    @Test
    public void testParse() {
        executor.execute(SqlRequests.of("CREATE TABLE IF NOT EXISTS test_table(" +
                "id varchar(32) primary key," +
                "name varchar(128) not null," +
                "age number(4)" +
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
                Assert.assertEquals(column.getPrecision(), 4);
                Assert.assertEquals(column.getScale(), 0);
                Assert.assertEquals(column.getDataType(), "decimal(4,0)");
                Assert.assertEquals(column.getJdbcType(), JDBCType.DECIMAL);
                Assert.assertEquals(column.getJavaType(), BigDecimal.class);
            }
        }finally {
            executor.execute(SqlRequests.of("drop table test_table"));
        }

    }

}