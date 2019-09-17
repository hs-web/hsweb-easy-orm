package org.hswebframework.ezorm.rdb.supports.h2;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;
import java.sql.JDBCType;

public class H2TableMetaParserTest {

    private H2TableMetadataParser parser;

    private SyncSqlExecutor executor;

    @Before
    @SneakyThrows
    public void init() {


        executor = new TestSyncSqlExecutor(new H2ConnectionProvider());
        H2SchemaMetadata schema = new H2SchemaMetadata("PUBLIC");
        schema.addFeature(executor);

        parser=new H2TableMetadataParser(schema);

    }

    @Test
    public void testParse() {
        executor.execute(SqlRequests.of("CREATE TABLE IF NOT EXISTS test_table(" +
                "id varchar(32) primary key," +
                "name varchar(128) not null," +
                "age number(4)" +
                ")"));
        try {
            RDBTableMetadata metaData = parser.parseByName("test_table").orElseThrow(NullPointerException::new);

            //id
            {
                RDBColumnMetadata column = metaData.getColumn("id").orElseThrow(NullPointerException::new);

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
                RDBColumnMetadata column = metaData.getColumn("name").orElseThrow(NullPointerException::new);

                Assert.assertNotNull(column);

                Assert.assertEquals(column.getDataType(), "varchar(128)");
                Assert.assertEquals(column.getLength(), 128);
                Assert.assertEquals(column.getJdbcType(), JDBCType.VARCHAR);
                Assert.assertEquals(column.getJavaType(), String.class);
                Assert.assertTrue(column.isNotNull());
            }

            //age
            {
                RDBColumnMetadata column = metaData.getColumn("age").orElseThrow(NullPointerException::new);

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