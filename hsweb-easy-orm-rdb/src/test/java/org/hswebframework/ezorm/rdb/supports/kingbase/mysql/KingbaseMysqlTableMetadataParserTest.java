package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.junit.Assert;
import org.junit.Test;

public class KingbaseMysqlTableMetadataParserTest {

    @Test
    public void shouldSupportCaseInsensitiveLookupForMixedCaseColumns() {
        System.out.println("[字段大小写查询] 验证混合大小写字段名是否支持不区分大小写查询");
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
        boolean lowerCaseMatched = table.getColumn("mixedname").orElse(null) == column;
        boolean upperCaseMatched = table.getColumn("MIXEDNAME").orElse(null) == column;
        boolean mixedCaseMatched = table.getColumn("MiXeDnAmE").orElse(null) == column;
        System.out.println("查询结果：mixedname=" + lowerCaseMatched
            + "，MIXEDNAME=" + upperCaseMatched
            + "，MiXeDnAmE=" + mixedCaseMatched);
        Assert.assertTrue(lowerCaseMatched);
        Assert.assertTrue(upperCaseMatched);
        Assert.assertTrue(mixedCaseMatched);
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
        Assert.assertSame(column, table.getColumnNow("mixedname"));

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

    private static final class TestParser extends KingbaseMysqlTableMetadataParser {
        private TestParser(KingbaseMysqlSchemaMetadata schema) {
            super(schema);
        }

        private void apply(RDBColumnMetadata column, Record record) {
            super.applyColumnInfo(column, record);
        }
    }
}
