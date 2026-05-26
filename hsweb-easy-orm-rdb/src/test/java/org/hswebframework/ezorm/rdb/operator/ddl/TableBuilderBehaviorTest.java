package org.hswebframework.ezorm.rdb.operator.ddl;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

public class TableBuilderBehaviorTest {

    @Test
    public void testColumnBuilderDefinesBusinessMetadataAndCommitsToTable() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();
        RDBTableMetadata table = new RDBTableMetadata("device");
        table.setSchema(schema);
        DefaultTableBuilder tableBuilder = new DefaultTableBuilder(table);
        DefaultValue defaultValue = () -> "unknown";

        tableBuilder.comment("device table")
                    .alias("Device")
                    .addColumn("id")
                    .alias("ID")
                    .type(JdbcDataType.of(JDBCType.VARCHAR, String.class))
                    .dataType("varchar")
                    .length(64)
                    .notNull()
                    .primaryKey()
                    .comment("primary key")
                    .columnDef("varchar(64)")
                    .defaultValue(defaultValue)
                    .property("business", "deviceId")
                    .custom(column -> column.setScale(0))
                    .commit();

        RDBColumnMetadata id = table.getColumn("id").orElseThrow(AssertionError::new);
        Assert.assertEquals("Device", table.getAlias());
        Assert.assertEquals("device table", table.getComment());
        Assert.assertEquals("ID", id.getAlias());
        Assert.assertEquals("varchar(64)", id.getDataType());
        Assert.assertEquals(64, id.getLength());
        Assert.assertTrue(id.isNotNull());
        Assert.assertTrue(id.isPrimaryKey());
        Assert.assertEquals("primary key", id.getComment());
        Assert.assertEquals("varchar(64)", id.getColumnDefinition());
        Assert.assertSame(defaultValue, id.getDefaultValue());
        Assert.assertEquals("deviceId", id.getProperty("business").getValue());
    }

    @Test
    public void testTableBuilderRemoveDropAndForeignKeyDsl() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();
        RDBTableMetadata table = schema.getTable("test").orElseThrow(AssertionError::new).clone();
        DefaultTableBuilder builder = new DefaultTableBuilder(table);

        builder.addColumn("temp")
               .type(JdbcDataType.of(JDBCType.INTEGER, Integer.class))
               .length(10, 0)
               .commit()
               .removeColumn("temp")
               .dropColumn("name")
               .allowAlter(false)
               .autoLoad(false)
               .merge(false)
               .custom(t -> t.setComment("changed"));

        Assert.assertFalse(table.getColumn("temp").isPresent());
        Assert.assertFalse(table.getColumn("name").isPresent());
        Assert.assertEquals("changed", table.getComment());

        builder.foreignKey()
               .name("fk_detail")
               .alias("detail")
               .target("detail")
               .column("id", "id")
               .toMany()
               .autoJoin(true)
               .condition(q -> q.is("detail_id", "id").and("state", TermType.eq, "enabled"))
               .commit();

        Assert.assertEquals(1, table.getForeignKey().size());
        Assert.assertEquals("fk_detail", table.getForeignKey().get(0).getName());
        Assert.assertEquals("detail", table.getForeignKey().get(0).getTarget().getName());
        Assert.assertEquals(2, table.getForeignKey().get(0).getTerms().size());
    }
}
