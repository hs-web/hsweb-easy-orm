package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
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

    private static final class TestParser extends KingbaseMysqlTableMetadataParser {
        private TestParser(KingbaseMysqlSchemaMetadata schema) {
            super(schema);
        }

        private void apply(RDBColumnMetadata column, Record record) {
            super.applyColumnInfo(column, record);
        }
    }
}
