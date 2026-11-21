package org.hswebframework.ezorm.rdb.supports.kingbaseES.mysql;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Map;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.prepare;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.lowerCase;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.singleMap;

/** Upstream testParse at 96b61b0b, with reported-or-zero INT precision for Kingbase metadata. */
public class KingbaseMysqlOfficialTableMetadataTests {

    @ClassRule
    public static final KingbaseMysqlOfficialTestDatabase DATABASE = new KingbaseMysqlOfficialTestDatabase();

    private SyncSqlExecutor executor;

    private KingbaseMysqlTableMetadataParser parser;

    @Before
    public void init() {
        RDBSchemaMetadata schema = DATABASE.newSchema();

        executor = DATABASE.sync();
        schema.addFeature(executor);
        parser = new KingbaseMysqlTableMetadataParser(schema);
    }

    @Test
    public void testParse() {
        executor.execute(SqlRequests.of("CREATE TABLE IF NOT EXISTS test_table(" +
                "id varchar(32) primary key," +
                "name varchar(128) not null," +
                "age int" +
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
                Map<String, Object> raw = executor.select(SqlRequests.of(
                    "select numeric_precision from information_schema.columns "
                        + "where table_schema = ? and table_name = ? and column_name = ?",
                    DATABASE.newSchema().getName(), "test_table", "age"), lowerCase(singleMap()));
                Assert.assertNotNull(raw);
                Object precision = raw.get("numeric_precision");
                Assert.assertEquals(precision == null ? 0 : Integer.parseInt(String.valueOf(precision)),
                    column.getPrecision());
                Assert.assertEquals(column.getScale(), 0);
                Assert.assertEquals(column.getDataType(), "int");
                Assert.assertEquals(column.getSqlType(), JDBCType.INTEGER);
                Assert.assertEquals(column.getJavaType(), Integer.class);
            }
        } finally {
            executor.execute(prepare("drop table test_table;"));
        }

    }


}
