package org.hswebframework.ezorm.rdb.supports.postgres;

import org.junit.Assert;
import org.junit.Test;
import org.postgresql.util.PGobject;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

public class PostgresqlArrayVectorCodecTest {

    @Test
    public void testArrayAndVectorStringNumberAndObjectBranches() throws SQLException {
        Assert.assertArrayEquals(new String[]{"a", "b"}, (String[]) PostgresqlArrayType.TEXT_ARRAY.decode("{a,b}"));
        Assert.assertArrayEquals(new String[]{"a", null, "c"}, (String[]) PostgresqlArrayType.TEXT_ARRAY.decode("[a,null,c]"));
        Assert.assertArrayEquals(new Integer[]{1, 2, 3}, (Integer[]) PostgresqlArrayType.INTEGER_ARRAY.decode(new int[]{1, 2, 3}));
        Assert.assertArrayEquals(new Long[]{1L, 2L}, (Long[]) PostgresqlArrayType.BIGINT_ARRAY.decode(List.of(1, 2)));
        Assert.assertArrayEquals(new Short[]{1}, (Short[]) PostgresqlArrayType.SMALLINT_ARRAY.decode(1));

        PGobject pgObject = new PGobject();
        pgObject.setType("integer[]");
        pgObject.setValue("{4,5}");
        Assert.assertArrayEquals(new Integer[]{4, 5}, (Integer[]) PostgresqlArrayType.INTEGER_ARRAY.decode(pgObject));

        Assert.assertArrayEquals(new String[]{"a,b", "c\"d"}, (String[]) PostgresqlArrayType.TEXT_ARRAY.decode("{\"a,b\",\"c\\\"d\"}"));
        Assert.assertArrayEquals(new String[]{"{nested,kept}", "tail"}, (String[]) PostgresqlArrayType.TEXT_ARRAY.decode("{{nested,kept},tail}"));
        Assert.assertArrayEquals(new Integer[]{null, 2}, (Integer[]) PostgresqlArrayType.INTEGER_ARRAY.decode("{ ,2}"));
        Assert.assertArrayEquals(new Long[]{9L}, (Long[]) PostgresqlArrayType.BIGINT_ARRAY.decode(9.7d));
        Assert.assertArrayEquals(new String[]{"7"}, (String[]) PostgresqlArrayType.VARCHAR_ARRAY.decode(7));
        Assert.assertArrayEquals(new String[0], (String[]) PostgresqlArrayType.TEXT_ARRAY.decode(""));
        Assert.assertArrayEquals(new String[0], (String[]) PostgresqlArrayType.TEXT_ARRAY.decode("{}"));
    }

    @Test
    public void testVectorTypeStringCollectionArrayAndPgobjectBranches() throws Exception {
        Assert.assertArrayEquals(new Float[]{1.0f, 2.5f}, (Float[]) VectorType.VECTOR.decode(List.of(1, 2.5f)));
        Assert.assertArrayEquals(new Float[]{1.0f, 2.0f}, (Float[]) VectorType.VECTOR.decode(new float[]{1f, 2f}));
        Assert.assertArrayEquals(new Float[]{1.0f, 2.0f}, VectorType.toFloatArray("[1,2]"));
        Assert.assertArrayEquals(new Float[]{1.0f, 2.0f}, VectorType.toFloatArray(new Float[]{1f, 2f}));
        Assert.assertArrayEquals(new Float[]{5.0f}, VectorType.toFloatArray(5));
        Assert.assertNull(VectorType.toFloatArray(new Object()));

        PGobject pgObject = new PGobject();
        pgObject.setValue("{3,4}");
        Assert.assertArrayEquals(new Float[]{3.0f, 4.0f}, (Float[]) VectorType.VECTOR.decode(pgObject));

        Assert.assertArrayEquals(new Float[0], VectorType.toFloatArray(""));
        Assert.assertArrayEquals(new Float[0], VectorType.toFloatArray("[]"));
        Assert.assertArrayEquals(new Float[]{null, 2.0f}, VectorType.toFloatArray("[ ,2]"));
        Assert.assertEquals("halfvec(512)", VectorType.HALF_VECTOR.createColumnDataType(new org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata()));
        Assert.assertEquals("sparsevec(512)", VectorType.SPARSE_VECTOR.createColumnDataType(new org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata()));
    }
}
