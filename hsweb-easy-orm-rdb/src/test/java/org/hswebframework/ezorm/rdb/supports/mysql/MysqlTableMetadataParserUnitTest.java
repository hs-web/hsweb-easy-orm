package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

public class MysqlTableMetadataParserUnitTest {

    @Test
    public void testApplyColumnInfoKeepEnumLiteralCase() {
        MysqlSchemaMetadata schema = new MysqlSchemaMetadata("test");
        MysqlTableMetadataParserForTest parser = new MysqlTableMetadataParserForTest(schema);
        RDBTableMetadata table = schema.newTable("test_table");
        RDBColumnMetadata column = table.newColumn();

        parser.apply(column, Record.newRecord()
                                   .putValue("name", "period")
                                   .putValue("column_type", "enum('Day','Month')")
                                   .putValue("data_length", 5)
                                   .putValue("not_null", 1));

        Assert.assertEquals("enum('Day','Month')", column.getDataType());
        Assert.assertEquals(JDBCType.VARCHAR, column.getSqlType());
        Assert.assertEquals(String.class, column.getJavaType());
    }

    static class MysqlTableMetadataParserForTest extends MysqlTableMetadataParser {

        public MysqlTableMetadataParserForTest(MysqlSchemaMetadata schema) {
            super(schema);
        }

        void apply(RDBColumnMetadata column, Record record) {
            applyColumnInfo(column, record);
        }
    }
}
