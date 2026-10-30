package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.junit.Assert;
import org.junit.Test;

/**
 * Verifies metadata parsing behavior without requiring a local database.
 */
public class KingbaseOracleTableMetadataParserTest {

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
