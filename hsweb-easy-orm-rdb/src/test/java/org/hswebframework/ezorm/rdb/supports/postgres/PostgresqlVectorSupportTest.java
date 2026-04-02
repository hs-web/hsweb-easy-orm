package org.hswebframework.ezorm.rdb.supports.postgres;

import io.r2dbc.postgresql.codec.Vector;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.junit.Assert;
import org.junit.Test;
import org.postgresql.util.PGobject;
import org.testcontainers.shaded.com.google.common.collect.Lists;

import java.sql.SQLException;

public class PostgresqlVectorSupportTest {

    @Test
    public void testVectorCodec() throws SQLException {
        Assert.assertArrayEquals(new Float[]{1.1F, 2.2F, 3F}, (Float[]) VectorType.VECTOR.decode(Vector.of(1.1f,2.2f,3)));
        Assert.assertArrayEquals(new Float[]{1.1F, 2.2F, 3F}, (Float[]) VectorType.VECTOR.encode(Lists.newArrayList(1.1f, 2.2f, 3f)));
        Assert.assertArrayEquals(new Float[]{1.1F, 2.2F, 3F}, (Float[]) VectorType.VECTOR.encode(new double[]{1.1D, 2.2D, 3D}));
        Assert.assertArrayEquals(new Float[]{1.1F, 2.2F, 3F}, (Float[]) VectorType.VECTOR.encode(new float[]{1.1f, 2.2f, 3f}));
        Assert.assertArrayEquals(new Float[]{1.1F, 2.2F, 3F}, (Float[]) VectorType.VECTOR.encode(new Float[]{1.1f, 2.2f, 3f}));
        Assert.assertArrayEquals(new Float[]{1.1F, 2.2F, 3F}, (Float[]) VectorType.VECTOR.encode("[1.1,2.2,3]"));
        Assert.assertArrayEquals(new Float[]{1.1F, 2.2F, 3F}, (Float[]) VectorType.VECTOR.encode("1.1,2.2,3"));

        PGobject pgObject = new PGobject();
        pgObject.setType("vector");
        pgObject.setValue("[1.1,2.2,3]");
        Assert.assertArrayEquals(new Float[]{1.1F, 2.2F, 3F}, (Float[]) VectorType.VECTOR.decode(pgObject));
    }

    @Test
    public void testVectorColumnType() {
        RDBSchemaMetadata schema = createSchema();
        RDBTableMetadata table = schema.newTable("test_vector");
        schema.addTable(table);

        RDBColumnMetadata column = createVectorColumn(table, "embedding", 1536);

        Assert.assertEquals("vector(1536)", column.getDataType());
    }

    private RDBColumnMetadata createVectorColumn(RDBTableMetadata table, String name, int dimension) {
        RDBColumnMetadata column = table.newColumn();
        column.setName(name);
        column.setOwner(table);
        column.setType(table.getDialect().convertDataType("vector(" + dimension + ")"));
        table.addColumn(column);
        return column;
    }

    private RDBSchemaMetadata createSchema() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.POSTGRES);
        RDBSchemaMetadata schema = new PostgresqlSchemaMetadata("public");
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        return schema;
    }
}
