package org.hswebframework.ezorm.rdb.supports.kingbaseES.mysql;

import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlTableMetadataParser;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

public class KingbaseMysqlTableMetadataParserTest {

    @Test
    public void shouldPreserveReportedIntegerPrecisionAndDefaultNullToZero() {
        MysqlSchemaMetadata mysql = new MysqlSchemaMetadata("test");
        KingbaseMysqlSchemaMetadata kingbase = new KingbaseMysqlSchemaMetadata("test");
        TestMysqlParser mysqlParser = new TestMysqlParser(mysql);
        TestParser kingbaseParser = new TestParser(kingbase);
        Object[][] cases = {{10, 10}, {null, 0}};
        for (Object[] testCase : cases) {
            RDBColumnMetadata mysqlColumn = mysql.newTable("types").newColumn();
            RDBColumnMetadata kingbaseColumn = kingbase.newTable("types").newColumn();
            mysqlParser.apply(mysqlColumn, Record.newRecord().putValue("name", "age")
                .putValue("column_type", "int").putValue("data_precision", testCase[0]));
            kingbaseParser.apply(kingbaseColumn, Record.newRecord().putValue("name", "age")
                .putValue("column_type", "integer").putValue("data_precision", testCase[0]));

            Assert.assertEquals(testCase[1], mysqlColumn.getPrecision());
            Assert.assertEquals(testCase[1], kingbaseColumn.getPrecision());
            Assert.assertEquals(JDBCType.INTEGER, kingbaseColumn.getSqlType());
            Assert.assertEquals(Integer.class, kingbaseColumn.getJavaType());
        }
    }

    @Test
    public void shouldPreserveColumnAliasesLikeNativeMysql() {
        MysqlSchemaMetadata mysql = new MysqlSchemaMetadata("test");
        KingbaseMysqlSchemaMetadata kingbase = new KingbaseMysqlSchemaMetadata("test");
        TestMysqlParser mysqlParser = new TestMysqlParser(mysql);
        TestParser kingbaseParser = new TestParser(kingbase);
        for (String name : new String[]{"name", "NAME", "MixedName"}) {
            RDBColumnMetadata expected = mysql.newTable("account").newColumn();
            RDBColumnMetadata actual = kingbase.newTable("account").newColumn();
            mysqlParser.apply(expected, Record.newRecord().putValue("name", name).putValue("column_type", "varchar(32)"));
            kingbaseParser.apply(actual, Record.newRecord().putValue("name", name).putValue("column_type", "varchar(32)"));
            Assert.assertEquals(name, expected.getAlias());
            Assert.assertEquals(name, actual.getAlias());
            Assert.assertEquals(expected.getRealName(), actual.getRealName());
        }
    }

    @Test
    public void shouldMatchMysqlMappingsForKingbaseTypeNames() {
        KingbaseMysqlSchemaMetadata schema = new KingbaseMysqlSchemaMetadata("public");
        TestParser parser = new TestParser(schema);
        // Expected constants captured from native MySQL 8.4.11 with the upstream parser.
        // This supplements upstream tests; it does not derive expectations from either dialect.
        Object[][] names = {
            {"integer", JDBCType.INTEGER, Integer.class},
            {"integer(11)", JDBCType.INTEGER, Integer.class},
            {"numeric", JDBCType.DECIMAL, java.math.BigDecimal.class},
            {"numeric(12,2)", JDBCType.DECIMAL, java.math.BigDecimal.class},
            {"character varying(32)", JDBCType.VARCHAR, String.class},
            {"character(32)", JDBCType.OTHER, String.class},
            {"bpchar(32)", JDBCType.OTHER, String.class},
            {"float4", JDBCType.FLOAT, String.class},
            {"float8", JDBCType.DOUBLE, Double.class},
            {"double precision", JDBCType.DOUBLE, Double.class},
            {"float4(12,2)", JDBCType.OTHER, String.class},
            {"boolean", JDBCType.TINYINT, Byte.class},
            {"bool", JDBCType.TINYINT, Byte.class},
            {"tinyint", JDBCType.TINYINT, Byte.class},
            {"smallint", JDBCType.SMALLINT, String.class},
            {"mediumint", JDBCType.OTHER, String.class},
            {"bigint", JDBCType.BIGINT, Long.class},
            {"int unsigned", JDBCType.OTHER, String.class},
            {"tinytext", JDBCType.OTHER, String.class},
            {"text", JDBCType.CLOB, String.class},
            {"mediumtext", JDBCType.OTHER, String.class},
            {"longtext", JDBCType.LONGVARCHAR, String.class},
            {"binary(16)", JDBCType.BINARY, byte[].class},
            {"varbinary(16)", JDBCType.OTHER, String.class},
            {"tinyblob", JDBCType.OTHER, String.class},
            {"blob", JDBCType.BLOB, String.class},
            {"mediumblob", JDBCType.OTHER, String.class},
            {"longblob", JDBCType.OTHER, String.class},
            {"bit(8)", JDBCType.OTHER, String.class},
            {"json", JDBCType.OTHER, String.class},
            {"date", JDBCType.DATE, java.time.LocalDate.class},
            {"time(6)", JDBCType.TIME, java.time.LocalTime.class},
            {"datetime(6)", JDBCType.TIMESTAMP, java.sql.Date.class},
            {"timestamp(6)", JDBCType.TIMESTAMP, java.sql.Timestamp.class},
            {"year", JDBCType.DATE, java.sql.Date.class},
            {"unknown_type", JDBCType.OTHER, String.class},
            {"custom_type(12,2)", JDBCType.OTHER, String.class},
            {"Enum_123", JDBCType.OTHER, String.class},
            {"Set_123", JDBCType.OTHER, String.class},
            {"float4_custom", JDBCType.OTHER, String.class},
            {"integer unsigned", JDBCType.OTHER, String.class}
        };
        for (Object[] pair : names) {
            RDBColumnMetadata column = schema.newTable("types").newColumn();
            parser.apply(column, Record.newRecord().putValue("name", "value")
                .putValue("data_type", pair[0]).putValue("column_type", pair[0]));
            Assert.assertEquals(String.valueOf(pair[0]), pair[1], column.getSqlType());
            Assert.assertEquals(String.valueOf(pair[0]), pair[2], column.getJavaType());
        }
    }

    @Test
    public void shouldMatchMysqlForParameterizedFloatingPointTypes() {
        KingbaseMysqlSchemaMetadata schema = new KingbaseMysqlSchemaMetadata("public");
        TestParser parser = new TestParser(schema);
        Object[][] types = {
            {"float4", JDBCType.OTHER, String.class, "float(12,2)"},
            {"float8", JDBCType.DOUBLE, Double.class, "double"},
            {"double precision", JDBCType.DOUBLE, Double.class, "double"}
        };
        for (Object[] type : types) {
            RDBColumnMetadata column = schema.newTable("types").newColumn();
            parser.apply(column, Record.newRecord().putValue("name", "value")
                .putValue("column_type", type[0]).putValue("data_precision", "12").putValue("data_scale", "2"));
            Assert.assertEquals(String.valueOf(type[0]), type[1], column.getSqlType());
            Assert.assertEquals(String.valueOf(type[0]), type[2], column.getJavaType());
            Assert.assertEquals(type[3], column.getDataType());
            Assert.assertEquals(12, column.getPrecision());
            Assert.assertEquals(2, column.getScale());
        }
    }

    @Test
    public void shouldRecognizeEnumAndSetByCatalogKindAndPreserveUnknownTypes() {
        KingbaseMysqlSchemaMetadata schema = new KingbaseMysqlSchemaMetadata("public");
        TestParser parser = new TestParser(schema);
        Object[][] types = {
            {"e", "Enum_123", JDBCType.VARCHAR},
            {"l", "Enum_123", JDBCType.VARCHAR},
            {"y", "Set_123", JDBCType.VARCHAR},
            {"d", "Enum_123", JDBCType.OTHER},
            {"", "Enum_123", JDBCType.OTHER},
            {"", "Set_123", JDBCType.OTHER}
        };
        for (Object[] type : types) {
            RDBColumnMetadata column = schema.newTable("types").newColumn();
            parser.apply(column, Record.newRecord().putValue("name", "value")
                .putValue("data_type", "USER-DEFINED").putValue("column_type", type[1])
                .putValue("type_kind", type[0]));
            Assert.assertEquals(String.valueOf(type[0]), type[2], column.getSqlType());
            Assert.assertEquals(String.class, column.getJavaType());
        }
    }

    // Assertion contract from upstream MysqlTableMetadataParserUnitTest at 96b61b0b.
    @Test
    public void testApplyColumnInfoKeepEnumLiteralCase() {
        KingbaseMysqlSchemaMetadata schema = new KingbaseMysqlSchemaMetadata("test");
        TestParser parser = new TestParser(schema);
        RDBColumnMetadata column = schema.newTable("test_table").newColumn();
        parser.apply(column, Record.newRecord()
            .putValue("name", "period")
            .putValue("column_type", "enum('Day','Month')")
            .putValue("data_length", 5)
            .putValue("not_null", 1));

        Assert.assertEquals("enum('Day','Month')", column.getDataType());
        Assert.assertEquals(JDBCType.VARCHAR, column.getSqlType());
        Assert.assertEquals(String.class, column.getJavaType());
    }

    @Test
    public void shouldMatchNativeMysqlLookupForMixedCaseColumns() {
        KingbaseMysqlSchemaMetadata schema = new KingbaseMysqlSchemaMetadata("public");
        TestParser parser = new TestParser(schema);
        RDBColumnMetadata column = new RDBColumnMetadata();
        parser.apply(column, Record.newRecord()
            .putValue("name", "MixedName")
            .putValue("data_type", "character varying")
            .putValue("data_length", 64));

        System.out.println("解析字段：name=" + column.getName()
            + "，alias=" + column.getAlias()
            + "，length=" + column.getLength());

        RDBTableMetadata table = schema.newTable("account");
        table.addColumn(column);
        MysqlSchemaMetadata mysql = new MysqlSchemaMetadata("public");
        RDBColumnMetadata expectedColumn = mysql.newTable("account").newColumn();
        new TestMysqlParser(mysql).apply(expectedColumn, Record.newRecord()
            .putValue("name", "MixedName").putValue("column_type", "varchar(64)"));
        RDBTableMetadata expectedTable = mysql.newTable("account");
        expectedTable.addColumn(expectedColumn);
        Assert.assertSame(column, table.getColumnNow("MixedName"));
        for (String lookup : new String[]{"MixedName", "mixedname", "MIXEDNAME", "MiXeDnAmE"}) {
            Assert.assertEquals(lookup, expectedTable.getColumn(lookup).isPresent(), table.getColumn(lookup).isPresent());
        }
    }

    @Test
    public void shouldThrowWhenRequiredColumnDoesNotExist() {
        KingbaseMysqlSchemaMetadata schema = new KingbaseMysqlSchemaMetadata("public");
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());
        database.addSchema(schema);
        TestParser parser = new TestParser(schema);
        RDBColumnMetadata column = new RDBColumnMetadata();
        parser.apply(column, Record.newRecord()
            .putValue("name", "MixedName")
            .putValue("data_type", "character varying")
            .putValue("data_length", 64));

        RDBTableMetadata table = schema.newTable("account");
        table.addColumn(column);
        Assert.assertSame(column, table.getColumnNow("MixedName"));

        String missingColumn = "missing_column";
        System.out.println("[字段查找异常] 表=" + table.getFullName()
            + "，已解析字段=" + column.getName() + "，查找字段=" + missingColumn);
        IllegalStateException exception = Assert.assertThrows(
            IllegalStateException.class,
            () -> table.getColumnNow(missingColumn)
        );
        System.out.println("[实际异常] " + exception.getClass().getName() + ": " + exception.getMessage());
        Assert.assertEquals("column not found:" + missingColumn + " in " + table.getFullName(),
                            exception.getMessage());
        System.out.println("[验证结果] 异常类型正确，消息包含缺失字段名称及所属表，测试通过");
    }

    private static final class TestMysqlParser extends MysqlTableMetadataParser {
        private TestMysqlParser(MysqlSchemaMetadata schema) {
            super(schema);
        }

        private void apply(RDBColumnMetadata column, Record record) {
            super.applyColumnInfo(column, record);
        }
    }

    private static final class TestParser extends KingbaseMysqlTableMetadataParser {
        private TestParser(KingbaseMysqlSchemaMetadata schema) {
            super(schema);
        }

        private void apply(RDBColumnMetadata column, Record record) {
            super.applyColumnInfo(column, record);
        }
    }
}
