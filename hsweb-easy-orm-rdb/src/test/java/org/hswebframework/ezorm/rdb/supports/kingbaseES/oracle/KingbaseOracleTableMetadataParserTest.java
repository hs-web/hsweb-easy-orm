package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

/**
 * Verifies metadata parsing behavior without requiring a local database.
 */
public class KingbaseOracleTableMetadataParserTest {

    // Fixed assertions from upstream OracleTableMetaParserTest.testParse at 96b61b0b.
    @Test
    public void shouldParseOfficialOracleColumnMetadata() {
        KingbaseOracleSchemaMetadata schema = new KingbaseOracleSchemaMetadata("test");
        TestParser parser = new TestParser(schema);
        RDBColumnMetadata name = schema.newTable("test_table").newColumn();
        parser.apply(name, Record.newRecord().putValue("name", "NAME")
            .putValue("data_type", "varchar2").putValue("data_length", 128).putValue("not_null", 1));
        Assert.assertEquals("varchar2(128 char)", name.getDataType());
        Assert.assertEquals(128, name.getLength());
        Assert.assertEquals(JDBCType.VARCHAR, name.getSqlType());
        Assert.assertEquals(String.class, name.getJavaType());
        Assert.assertTrue(name.isNotNull());

        for (String type : new String[]{"number", "numeric"}) {
            RDBColumnMetadata age = schema.newTable("test_table").newColumn();
            parser.apply(age, Record.newRecord().putValue("name", "AGE")
                .putValue("data_type", type).putValue("data_precision", 10).putValue("data_scale", 0));
            Assert.assertEquals("number(10,0)", age.getDataType());
            Assert.assertEquals(10, age.getPrecision());
            Assert.assertEquals(0, age.getScale());
            Assert.assertEquals(JDBCType.NUMERIC, age.getSqlType());
            Assert.assertEquals(java.math.BigDecimal.class, age.getJavaType());
        }
    }

    @Test
    public void shouldDistinguishNativeDateFromTimestampMetadata() {
        KingbaseOracleSchemaMetadata schema = new KingbaseOracleSchemaMetadata("public");
        TestParser parser = new TestParser(schema);
        for (String type : new String[]{"date", "timestamp without time zone"}) {
            RDBColumnMetadata column = schema.newTable("account").newColumn();
            parser.apply(column, Record.newRecord()
                .putValue("name", "CREATED_AT")
                .putValue("data_type", "timestamp without time zone")
                .putValue("column_type", type));
            Assert.assertEquals(JDBCType.TIMESTAMP, column.getSqlType());
            Assert.assertEquals("date".equals(type) ? java.sql.Date.class : java.sql.Timestamp.class,
                column.getJavaType());
        }
    }

    @Test
    public void shouldReadCatalogQualifiedVarcharMetadata() {
        KingbaseOracleSchemaMetadata schema = new KingbaseOracleSchemaMetadata("public");
        TestParser parser = new TestParser(schema);
        for (String type : new String[]{"pg_catalog.varchar", "varchar", "varchar2"}) {
            RDBColumnMetadata column = schema.newTable("account").newColumn();
            parser.apply(column, Record.newRecord()
                .putValue("name", "NAME")
                .putValue("data_type", type)
                .putValue("column_type", type)
                .putValue("data_length", 64)
                .putValue("not_null", "1"));

            Assert.assertEquals(JDBCType.VARCHAR, column.getSqlType());
            Assert.assertEquals(String.class, column.getJavaType());
            Assert.assertEquals(64, column.getLength());
            Assert.assertTrue(column.isNotNull());
            Assert.assertEquals("varchar2(64 char)", column.getDataType());
        }
    }

    @Test
    public void shouldPreserveMetadataAndSupportUppercaseColumnLookup() {
        KingbaseOracleSchemaMetadata schema = new KingbaseOracleSchemaMetadata("public");
        TestParser parser = new TestParser(schema);
        RDBColumnMetadata column = new RDBColumnMetadata();

        parser.apply(column, Record.newRecord()
            .putValue("name", "account_name")
            .putValue("data_type", "varchar")
            .putValue("data_length", 64)
            .putValue("not_null", "1"));

        Assert.assertEquals("account_name", column.getName());
        Assert.assertEquals(64, column.getLength());
        Assert.assertTrue(column.isNotNull());

        RDBTableMetadata table = schema.newTable("ACCOUNT");
        table.addColumn(column);
        Assert.assertSame(column, table.getColumn("ACCOUNT_NAME").orElse(null));
    }

    private static final class TestParser extends KingbaseOracleTableMetadataParser {

        private TestParser(KingbaseOracleSchemaMetadata schema) {
            super(schema);
        }

        private void apply(RDBColumnMetadata column, Record record) {
            super.applyColumnInfo(column, record);
        }
    }
}
