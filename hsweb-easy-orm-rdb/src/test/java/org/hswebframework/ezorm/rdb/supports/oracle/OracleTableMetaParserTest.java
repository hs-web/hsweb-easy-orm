package org.hswebframework.ezorm.rdb.supports.oracle;

import io.r2dbc.spi.Statement;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import reactor.test.StepVerifier;

import java.math.BigDecimal;
import java.sql.JDBCType;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.prepare;

public class OracleTableMetaParserTest {

    OracleSchemaMetadata schema;

    private SyncSqlExecutor executor;

    private OracleTableMetadataParser parser;

    @Before
    public void init() {
        executor = new TestSyncSqlExecutor(new OracleConnectionProvider());
        schema = new OracleSchemaMetadata("SYSTEM");
        schema.addFeature(executor);
        schema.addFeature(new TestReactiveSqlExecutor(":",new OracleR2dbcConnectionProvider()){
            @Override
            protected void bind(Statement statement, int index, Object value) {
                statement.bind(index ,value);
            }

            @Override
            protected void bindNull(Statement statement, int index, Class<?> type) {
                statement.bindNull(index ,type);
            }
        });

        parser = new OracleTableMetadataParser(schema);
    }

    @Test
    public void testParseReactive() {
        schema.loadAllTableReactive()
              .as(StepVerifier::create)
              .expectComplete()
              .verify();

        System.out.println(1);
    }

    @Test
    public void testParse() {

        try {
            executor.execute(SqlRequests.of("CREATE TABLE test_table(" +
                                                    "id varchar2(32) primary key," +
                                                    "name varchar2(128) not null," +
                                                    "age number(10)" +
                                                    ")"));
            RDBTableMetadata metaData = parser.parseByName("test_table").orElseThrow(NullPointerException::new);

            //id
            {
                RDBColumnMetadata column = metaData.getColumn("id").orElseThrow(NullPointerException::new);

                Assert.assertNotNull(column);

                Assert.assertEquals(column.getDataType(), "varchar2(32)");
                Assert.assertEquals(column.getType().getSqlType(), JDBCType.VARCHAR);
                Assert.assertEquals(column.getJavaType(), String.class);
                Assert.assertTrue(column.isNotNull());
                // 这里只解析表结构，而不会解析键信息.
                // Assert.assertTrue(column.isPrimaryKey());
            }

            //name
            {
                RDBColumnMetadata column = metaData.getColumn("name").orElseThrow(NullPointerException::new);

                Assert.assertNotNull(column);

                Assert.assertEquals(column.getDataType(), "varchar2(128)");
                Assert.assertEquals(column.getLength(), 128);
                Assert.assertEquals(column.getType().getSqlType(), JDBCType.VARCHAR);
                Assert.assertEquals(column.getJavaType(), String.class);
                Assert.assertTrue(column.isNotNull());
            }

            //age
            {
                RDBColumnMetadata column = metaData.getColumn("age").orElseThrow(NullPointerException::new);

                Assert.assertNotNull(column);
                Assert.assertEquals(column.getPrecision(), 10);
                Assert.assertEquals(column.getScale(), 0);
                Assert.assertEquals(column.getDataType(), "number(10,0)");
                Assert.assertEquals(column.getType().getSqlType(), JDBCType.NUMERIC);
                Assert.assertEquals(column.getJavaType(), BigDecimal.class);
            }
        } finally {
            executor.execute(prepare("drop table test_table"));
        }

    }


}