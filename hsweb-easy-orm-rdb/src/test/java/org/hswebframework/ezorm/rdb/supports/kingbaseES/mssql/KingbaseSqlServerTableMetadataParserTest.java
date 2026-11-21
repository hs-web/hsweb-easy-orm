package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServer2012TableMetadataParser;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Date;
import java.util.Collections;
import java.util.Map;
import reactor.test.StepVerifier;

public class KingbaseSqlServerTableMetadataParserTest {

    private final KingbaseSqlServerSchemaMetadata schema = new KingbaseSqlServerSchemaMetadata("dbo");
    private final TestParser parser = new TestParser(schema);

    // Fixed column assertions from supports/mssql/MSSQLTableMetaParserTest.testParse.
    // These are unit fixtures, not a claim that the catalog SQL has run against KingbaseES.
    @Test
    public void shouldMatchOfficialSqlServerColumnMetadata() {
        RDBColumnMetadata id = parse(Record.newRecord().putValue("name", "id")
            .putValue("data_type", "varchar").putValue("data_length", 32)
            .putValue("not_null", 1).putValue("primary_key", 1));
        Assert.assertEquals("varchar(32)", id.getDataType());
        Assert.assertEquals(JDBCType.VARCHAR, id.getSqlType());
        Assert.assertEquals(String.class, id.getJavaType());
        Assert.assertTrue(id.isNotNull());
        Assert.assertTrue(id.isPrimaryKey());

        RDBColumnMetadata name = parse(Record.newRecord().putValue("name", "name")
            .putValue("data_type", "varchar").putValue("data_length", 128).putValue("not_null", 1));
        Assert.assertEquals("varchar(128)", name.getDataType());
        Assert.assertEquals(128, name.getLength());
        Assert.assertEquals(JDBCType.VARCHAR, name.getSqlType());
        Assert.assertEquals(String.class, name.getJavaType());
        Assert.assertTrue(name.isNotNull());

        RDBColumnMetadata age = parse(Record.newRecord().putValue("name", "age")
            .putValue("data_type", "int").putValue("data_precision", 10).putValue("data_scale", 0));
        Assert.assertEquals(10, age.getPrecision());
        Assert.assertEquals(0, age.getScale());
        Assert.assertEquals("int", age.getDataType());
        Assert.assertEquals(JDBCType.INTEGER, age.getSqlType());
        Assert.assertEquals(Integer.class, age.getJavaType());

        RDBColumnMetadata time = parse(Record.newRecord().putValue("name", "create_time")
            .putValue("data_type", "datetime2"));
        Assert.assertEquals("datetime2", time.getDataType());
        Assert.assertEquals(JDBCType.TIMESTAMP, time.getSqlType());
        Assert.assertEquals(Date.class, time.getJavaType());
    }

    @Test
    public void shouldNormalizeStringDimensionsAndPreserveColumnNames() {
        RDBColumnMetadata column = parse(Record.newRecord().putValue("name", "AccountName")
            .putValue("data_type", "character varying").putValue("data_length", "64")
            .putValue("not_null", "1").putValue("comment", "账户名"));
        Assert.assertEquals("AccountName", column.getName());
        Assert.assertEquals("AccountName", column.getRealName());
        Assert.assertEquals(64, column.getLength());
        Assert.assertEquals("varchar(64)", column.getDataType());
        Assert.assertEquals("账户名", column.getComment());
        Assert.assertTrue(column.isNotNull());

        RDBColumnMetadata amount = parse(Record.newRecord().putValue("name", "amount")
            .putValue("data_type", "numeric").putValue("data_precision", "18").putValue("data_scale", "4"));
        Assert.assertEquals(18, amount.getPrecision());
        Assert.assertEquals(4, amount.getScale());
        Assert.assertEquals("numeric(18,4)", amount.getDataType());
    }

    @Test
    public void shouldScopeCatalogQueriesAndReadOnlyDescriptionProperties() {
        Assert.assertTrue(parser instanceof SqlServer2012TableMetadataParser);
        String columns = parser.getTableMetaSql("account");
        Assert.assertTrue(columns.contains("cols.TABLE_SCHEMA = #{schema}"));
        Assert.assertTrue(columns.contains("cols.TABLE_NAME LIKE #{table}"));
        Assert.assertTrue(columns.contains("tc.CONSTRAINT_TYPE = 'PRIMARY KEY'"));
        Assert.assertTrue(columns.contains("k.TABLE_NAME = cols.TABLE_NAME"));
        Assert.assertTrue(columns.contains("k.TABLE_SCHEMA = cols.TABLE_SCHEMA"));
        Assert.assertFalse(columns.contains("LIKE 'PK%"));
        Assert.assertTrue(columns.contains("col_description(t.object_id, c.column_id)"));
        Assert.assertTrue(columns.contains("format_type(a.atttypid, a.atttypmod)"));
        Assert.assertTrue(columns.contains("c.precision"));
        Assert.assertTrue(parser.getTableCommentSql("account").contains("ep.minor_id = 0"));
        Assert.assertTrue(parser.getAllTableSql().contains("SCHEMA_ID(#{schema})"));
        Assert.assertTrue(parser.getTableExistsSql().contains("SCHEMA_ID(#{schema})"));
        Assert.assertTrue(parser.getTableExistsSql().contains("#{table}"));
    }

    @Test
    public void shouldParseByNameThroughSyncAndReactiveResultWrappers() {
        attachCatalog();
        assertTable(parser.parseByName("account").orElseThrow(AssertionError::new));
        parser.parseByNameReactive("account").as(StepVerifier::create)
            .assertNext(KingbaseSqlServerTableMetadataParserTest::assertTable).verifyComplete();
        Assert.assertFalse(parser.parseByName("missing").isPresent());
        parser.parseByNameReactive("missing").as(StepVerifier::create).verifyComplete();
    }

    @Test
    public void shouldRetainInheritedFastParseAllForSyncAndReactive() {
        attachCatalog();
        Assert.assertEquals(Collections.singletonList("account"), parser.parseAllTableName());
        parser.parseAllTableNameReactive().as(StepVerifier::create).expectNext("account").verifyComplete();
        Assert.assertEquals(1, parser.parseAll().size());
        assertTable(parser.parseAll().get(0));
        parser.parseAllReactive().as(StepVerifier::create)
            .assertNext(KingbaseSqlServerTableMetadataParserTest::assertTable).verifyComplete();
    }

    @Test
    public void shouldBindSchemaAndTableNamesAsParameters() {
        KingbaseSqlServerCatalogExecutor executor = attachCatalog();
        String name = "account' OR 1=1 --";
        parser.tableExists(name);
        Assert.assertEquals(1, executor.requests.size());
        Assert.assertArrayEquals(new Object[]{"dbo", name}, executor.requests.get(0).getParameters());
        Assert.assertFalse(executor.requests.get(0).getSql().contains(name));
    }

    @Test
    public void shouldKeepAbsentTableCommentsNullForSyncAndReactive() {
        attachCatalog(false);
        // An inner join returns no comment row when MS_Description is absent.
        Assert.assertFalse(parser.getTableCommentSql("account").contains("LEFT JOIN"));
        Assert.assertNull(parser.parseByName("account").orElseThrow(AssertionError::new).getComment());
        parser.parseByNameReactive("account").as(StepVerifier::create)
            .assertNext(table -> Assert.assertNull(table.getComment())).verifyComplete();
    }

    private KingbaseSqlServerCatalogExecutor attachCatalog() {
        return attachCatalog(true);
    }

    private KingbaseSqlServerCatalogExecutor attachCatalog(boolean withComment) {
        KingbaseSqlServerCatalogExecutor executor = new KingbaseSqlServerCatalogExecutor(sql -> {
            if (sql.getSql().contains("count(1)")) {
                return Collections.singletonList(Collections.singletonMap("total",
                    java.util.Arrays.asList(sql.getParameters()).contains("missing") ? 0 : 1));
            }
            if (sql.getSql().contains("INFORMATION_SCHEMA.COLUMNS")) {
                return Collections.singletonList(Map.of("table_name", "account", "name", "id",
                    "data_type", "varchar", "data_length", "32", "not_null", 1, "primary_key", 1));
            }
            if (sql.getSql().contains("sys.extended_properties")) {
                return withComment ? Collections.singletonList(Map.of("table_name", "account", "comment", "账户表"))
                    : Collections.emptyList();
            }
            if (sql.getSql().contains("SELECT name as [name]")) {
                return Collections.singletonList(Collections.singletonMap("name", "account"));
            }
            return Collections.emptyList();
        });
        schema.addFeature(executor);
        schema.addFeature(executor.reactive());
        return executor;
    }

    private static void assertTable(RDBTableMetadata table) {
        Assert.assertEquals("account", table.getName());
        Assert.assertEquals("账户表", table.getComment());
        Assert.assertEquals(1, table.getColumns().size());
        RDBColumnMetadata id = table.getColumn("id").orElseThrow(AssertionError::new);
        Assert.assertEquals("varchar(32)", id.getDataType());
        Assert.assertTrue(id.isPrimaryKey());
        Assert.assertTrue(id.isNotNull());
    }

    private RDBColumnMetadata parse(Record record) {
        RDBColumnMetadata column = schema.newTable("test_table").newColumn();
        parser.applyColumnInfo(column, record);
        return column;
    }

    private static class TestParser extends KingbaseSqlServerTableMetadataParser {
        TestParser(RDBSchemaMetadata schema) {
            super(schema);
        }
    }
}
