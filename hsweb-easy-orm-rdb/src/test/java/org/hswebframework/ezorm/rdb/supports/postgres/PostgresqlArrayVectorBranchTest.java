package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Test;
import org.postgresql.util.PGobject;

import java.sql.ResultSet;
import java.util.List;

public class PostgresqlArrayVectorBranchTest {

    @Test
    public void testPostgresqlArrayTypeBranches() throws Exception {
        PostgresqlArrayType type = PostgresqlArrayType.INTEGER_ARRAY;
        RDBColumnMetadata column = column(type);

        Assert.assertArrayEquals(new Integer[]{1, 2}, (Integer[]) type.decode(List.of(1, 2)));
        Assert.assertArrayEquals(new Integer[]{1, 2}, (Integer[]) type.decode(new Integer[]{1, 2}));
        Assert.assertArrayEquals(new Integer[]{1, 2}, (Integer[]) type.decode("{1,2}"));
        Assert.assertArrayEquals(new Integer[]{1, 2}, (Integer[]) type.decode("(1,2)"));
        Assert.assertArrayEquals(new Integer[]{1, 2}, (Integer[]) type.decode("[1,2]"));
        Assert.assertArrayEquals(new Integer[]{1, null, 3}, (Integer[]) type.decode("{1,NULL,3}"));
        Assert.assertArrayEquals(new Integer[]{1, 2}, (Integer[]) type.decode(pgArray("{1,2}")));
        PGobject object = new PGobject();
        object.setType("integer[]");
        object.setValue("{3,4}");
        Assert.assertArrayEquals(new Integer[]{3, 4}, (Integer[]) type.decode(object));
        Assert.assertArrayEquals(new Integer[]{5}, (Integer[]) type.decode(5));
        Assert.assertNull(type.decode(null));
        Assert.assertEquals("integer[]", type.createColumnDataType(column));
        Assert.assertTrue(type.encode(List.of(1, 2)) instanceof Integer[]);
        Assert.assertTrue(type.encode(List.of(1, 2), column) instanceof PostgresqlArrayParameter);
    }

    @Test
    public void testVectorTypeBranches() throws Exception {
        VectorType type = VectorType.VECTOR;
        RDBTableMetadata table = new H2SchemaMetadata("PUBLIC").newTable("vector_test");
        RDBColumnMetadata column = table.newColumn();
        column.setName("v");

        Assert.assertArrayEquals(new Float[]{1.0f, 2.5f}, (Float[]) type.decode(List.of(1, 2.5f)));
        Assert.assertArrayEquals(new Float[]{1.0f, 2.0f}, (Float[]) type.decode(new float[]{1f, 2f}));
        Assert.assertArrayEquals(new Float[]{1.0f, 2.0f}, (Float[]) type.decode("[1,2]"));
        PGobject object = new PGobject();
        object.setValue("{3,4}");
        Assert.assertArrayEquals(new Float[]{3.0f, 4.0f}, (Float[]) type.decode(object));
        Assert.assertArrayEquals(new Float[]{5.0f}, (Float[]) type.decode(5));
        Assert.assertNull(type.decode(null));
        Assert.assertEquals("vector(512)", type.createColumnDataType(column));
        column.setLength(128);
        Assert.assertEquals("vector(128)", type.createColumnDataType(column));
    }

    private static RDBColumnMetadata column(DataType type) {
        RDBTableMetadata table = new H2SchemaMetadata("PUBLIC").newTable("array_test");
        RDBColumnMetadata column = table.newColumn();
        column.setName("arr");
        column.setType(type);
        return column;
    }

    private static java.sql.Array pgArray(String literal) {
        return new java.sql.Array() {
            @Override
            public String getBaseTypeName() { return "integer"; }
            @Override
            public int getBaseType() { return java.sql.Types.INTEGER; }
            @Override
            public Object getArray() { return literal == null ? new Integer[0] : new Integer[]{1, 2}; }
            @Override
            public Object getArray(java.util.Map<String, Class<?>> map) { return getArray(); }
            @Override
            public Object getArray(long index, int count) { return getArray(); }
            @Override
            public Object getArray(long index, int count, java.util.Map<String, Class<?>> map) { return getArray(); }
            @Override
            public ResultSet getResultSet() { return null; }
            @Override
            public ResultSet getResultSet(java.util.Map<String, Class<?>> map) { return null; }
            @Override
            public ResultSet getResultSet(long index, int count) { return null; }
            @Override
            public ResultSet getResultSet(long index, int count, java.util.Map<String, Class<?>> map) { return null; }
            @Override
            public void free() { }
        };
    }
}
