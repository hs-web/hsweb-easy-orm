package org.hswebframework.ezorm.rdb.supports.postgres;

import io.r2dbc.postgresql.codec.Vector;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Test;
import org.postgresql.util.PGobject;

public class VectorTypeBranchTest {

    @Test
    public void testToFloatArrayBranches() throws Exception {
        Assert.assertArrayEquals(new Float[]{1f, 2f}, VectorType.toFloatArray(java.util.List.of(1, 2)));
        Assert.assertArrayEquals(new Float[]{1f, 2f}, VectorType.toFloatArray(new int[]{1, 2}));
        Assert.assertArrayEquals(new Float[]{1f, 2f}, VectorType.toFloatArray("[1,2]"));
        Assert.assertArrayEquals(new Float[]{1f}, VectorType.toFloatArray(1));
        Assert.assertNull(VectorType.toFloatArray(null));

        VectorType type = VectorType.VECTOR;
        Assert.assertArrayEquals(new Float[]{3f, 4f}, (Float[]) type.decode(Vector.of(3f, 4f)));
        PGobject obj = new PGobject(); obj.setValue("{5,6}"); obj.setType("vector");
        Assert.assertArrayEquals(new Float[]{5f, 6f}, (Float[]) type.decode(obj));
        RDBColumnMetadata column = new H2SchemaMetadata("PUBLIC").newTable("v").newColumn(); column.setName("v");
        Assert.assertEquals("vector(512)", type.createColumnDataType(column));
    }
}
