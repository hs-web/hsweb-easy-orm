package org.hswebframework.ezorm.rdb.metadata;

import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.core.meta.DefaultFeatureType;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Optional;

public class RDBColumnMetadataBehaviorTest {

    @Test
    public void testDerivedLengthPrecisionJavaTypeAndNotNull() {
        RDBTableMetadata table = createTable();
        RDBColumnMetadata column = table.newColumn();
        column.setName("amount");
        column.setOwner(table);
        column.setType(JdbcDataType.of(JDBCType.DECIMAL, Number.class));

        Assert.assertEquals(10, column.getLength(10));
        Assert.assertEquals(0, column.getLength());
        Assert.assertEquals(18, column.getPrecision(18));
        Assert.assertEquals(0, column.getPrecision());
        Assert.assertEquals(Number.class, column.getJavaType());
        Assert.assertFalse(column.isNotNull());

        column.setPrimaryKey(true);
        Assert.assertTrue(column.isNotNull());
    }

    @Test
    public void testLengthSupportTypePopulatesLengthPrecisionScale() {
        RDBTableMetadata table = createTable();
        RDBColumnMetadata column = table.newColumn();
        column.setName("name");
        column.setOwner(table);
        column.setType(new LengthType());

        Assert.assertEquals(32, column.getLength());
        Assert.assertEquals(18, column.getPrecision());
        Assert.assertEquals(6, column.getScale());
        Assert.assertEquals("length(32)", column.getDataType());
    }

    @Test
    public void testDdlModifiableAndGenerateDefaultValue() {
        RDBTableMetadata table = createTable();
        RDBColumnMetadata before = table.newColumn();
        before.setName("score");
        before.setOwner(table);
        before.setType(JdbcDataType.of(JDBCType.DECIMAL, Number.class));
        before.setPrecision(10);
        before.setScale(2);
        before.getPreviousName();

        RDBColumnMetadata same = before.clone();
        same.setOwner(table);
        same.setType(JdbcDataType.of(JDBCType.DECIMAL, Number.class));
        same.setPrecision(10);
        same.setScale(2);
        Assert.assertFalse(before.ddlModifiable(same));

        RDBColumnMetadata larger = before.clone();
        larger.setOwner(table);
        larger.setType(JdbcDataType.of(JDBCType.DECIMAL, Number.class));
        larger.setPrecision(12);
        larger.setScale(4);
        Assert.assertTrue(before.ddlModifiable(larger));

        RDBColumnMetadata renamed = before.clone();
        renamed.setOwner(table);
        renamed.setName("score_new");
        Assert.assertFalse(before.ddlModifiable(renamed));

        RDBColumnMetadata defaultValueColumn = table.newColumn();
        defaultValueColumn.setName("created_at");
        defaultValueColumn.setOwner(table);
        defaultValueColumn.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        defaultValueColumn.setDefaultValue((RuntimeDefaultValue) () -> "2026-05-26 12:00:00");
        Assert.assertEquals(Optional.of("2026-05-26 12:00:00"), defaultValueColumn.generateDefaultValue());
    }

    @Test
    public void testToStringFeatureAndFullNameBranches() {
        RDBTableMetadata table = createTable();
        RDBColumnMetadata column = table.newColumn();
        column.setName("nick_name");
        column.setAlias("nick.name");
        column.setComment("昵称");
        column.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        table.addColumn(column);

        Assert.assertTrue(column.toString().contains("nick_name"));
        Assert.assertTrue(column.toString().contains("String"));
        Assert.assertTrue(column.toString().contains("昵称"));
        Assert.assertEquals(column.getFullName(), column.getFullName(null));
        Assert.assertEquals(column.getFullName(), column.getFullName(""));
        Assert.assertTrue(column.getFullName("alias").contains("alias"));
        Assert.assertTrue(column.getFullTableName().contains("test_column_behavior"));
        Assert.assertEquals(RDBObjectType.column, column.getObjectType());

        Feature feature = new SimpleFeature("column-feature");
        column.addFeature(feature);
        Assert.assertSame(feature, column.findFeatureOrElse("column-feature", () -> null));
        Assert.assertEquals(1, column.findFeatures(f -> f.getId().equals("column-feature")).size());
    }

    @Test
    public void testDdlModifiableLengthScaleAndNotNullBranches() {
        RDBTableMetadata table = createTable();
        RDBColumnMetadata before = table.newColumn();
        before.setName("name");
        before.setOwner(table);
        before.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        before.setLength(16);
        before.getPreviousName();

        RDBColumnMetadata longer = before.clone();
        longer.setOwner(table);
        longer.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        longer.setLength(32);
        Assert.assertTrue(before.ddlModifiable(longer));

        RDBColumnMetadata shorter = before.clone();
        shorter.setOwner(table);
        shorter.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        shorter.setLength(8);
        Assert.assertFalse(before.ddlModifiable(shorter));

        RDBColumnMetadata notNull = before.clone();
        notNull.setOwner(table);
        notNull.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        notNull.setNotNull(true);
        Assert.assertTrue(before.ddlModifiable(notNull));

        RDBColumnMetadata scaleBefore = table.newColumn();
        scaleBefore.setName("amount");
        scaleBefore.setOwner(table);
        scaleBefore.setType(new ScaleOnlyType());
        scaleBefore.setScale(2);
        scaleBefore.setDataType(null);
        RDBColumnMetadata scaleAfter = scaleBefore.clone();
        scaleAfter.setOwner(table);
        scaleAfter.setType(new ScaleOnlyType());
        scaleAfter.setScale(4);
        scaleAfter.setDataType(null);
        Assert.assertFalse(scaleBefore.ddlModifiable(scaleAfter));
    }

    private RDBTableMetadata createTable() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        RDBSchemaMetadata schema = new H2SchemaMetadata("PUBLIC");
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        RDBTableMetadata table = schema.newTable("test_column_behavior");
        schema.addTable(table);
        return table;
    }

    private static class LengthType implements DataType, LengthSupport, org.hswebframework.ezorm.rdb.metadata.dialect.DataTypeBuilder {
        @Override
        public String getId() {
            return "length";
        }

        @Override
        public String getName() {
            return "length";
        }

        @Override
        public java.sql.SQLType getSqlType() {
            return JDBCType.VARCHAR;
        }

        @Override
        public Class<?> getJavaType() {
            return String.class;
        }

        @Override
        public int getLength() {
            return 32;
        }

        @Override
        public int getScale() {
            return 6;
        }

        @Override
        public int getPrecision() {
            return 18;
        }

        @Override
        public String createColumnDataType(RDBColumnMetadata columnMetaData) {
            return "length(" + columnMetaData.getLength(32) + ")";
        }
    }

    private record SimpleFeature(String id) implements Feature {
        @Override
        public String getId() {
            return id;
        }

        @Override
        public String getName() {
            return id;
        }

        @Override
        public org.hswebframework.ezorm.core.FeatureType getType() {
            return DefaultFeatureType.metadataParser;
        }
    }

    private static class ScaleOnlyType implements DataType {
        @Override
        public String getId() {
            return "scale-only";
        }

        @Override
        public String getName() {
            return "scale-only";
        }

        @Override
        public java.sql.SQLType getSqlType() {
            return JDBCType.OTHER;
        }

        @Override
        public Class<?> getJavaType() {
            return String.class;
        }

        @Override
        public boolean isScaleSupport() {
            return true;
        }
    }
}
