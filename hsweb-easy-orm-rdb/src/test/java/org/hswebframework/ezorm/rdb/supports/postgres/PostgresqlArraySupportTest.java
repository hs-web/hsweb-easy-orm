package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.junit.Assert;
import org.junit.Test;
import org.postgresql.util.PGobject;

import java.util.List;

public class PostgresqlArraySupportTest {

    @Test
    public void testStringArrayCodec() throws Exception {
        Assert.assertArrayEquals(new String[]{"a", "b"}, (String[]) PostgresqlArrayType.VARCHAR_ARRAY.encode(List.of("a", "b")));
        Assert.assertArrayEquals(new String[]{"a", "b"}, (String[]) PostgresqlArrayType.VARCHAR_ARRAY.encode(new String[]{"a", "b"}));
        Assert.assertArrayEquals(new String[]{"a", "b"}, (String[]) PostgresqlArrayType.VARCHAR_ARRAY.decode("{a,b}"));
        Assert.assertArrayEquals(new String[]{"white shirt", "glasses", null}, (String[]) PostgresqlArrayType.TEXT_ARRAY.decode("{\"white shirt\",glasses,NULL}"));
        Assert.assertArrayEquals(new String[]{"NULL", "a,b"}, (String[]) PostgresqlArrayType.TEXT_ARRAY.decode("{\"NULL\",\"a,b\"}"));

        PGobject pgObject = new PGobject();
        pgObject.setType("text[]");
        pgObject.setValue("{\"white shirt\",glasses}");
        Assert.assertArrayEquals(new String[]{"white shirt", "glasses"}, (String[]) PostgresqlArrayType.TEXT_ARRAY.decode(pgObject));
    }

    @Test
    public void testNumberArrayCodec() {
        Assert.assertArrayEquals(new Short[]{1, 2, 3}, (Short[]) PostgresqlArrayType.SMALLINT_ARRAY.encode(new short[]{1, 2, 3}));
        Assert.assertArrayEquals(new Short[]{1, 2, 3}, (Short[]) PostgresqlArrayType.SMALLINT_ARRAY.decode("{1,2,3}"));
        Assert.assertArrayEquals(new Integer[]{1, 2, 3}, (Integer[]) PostgresqlArrayType.INTEGER_ARRAY.encode(List.of(1L, 2L, 3L)));
        Assert.assertArrayEquals(new Integer[]{1, 2, 3}, (Integer[]) PostgresqlArrayType.INTEGER_ARRAY.decode("{1,2,3}"));
        Assert.assertArrayEquals(new Long[]{1L, 2L, 3L}, (Long[]) PostgresqlArrayType.BIGINT_ARRAY.encode(new int[]{1, 2, 3}));
        Assert.assertArrayEquals(new Long[]{1L, null, 3L}, (Long[]) PostgresqlArrayType.BIGINT_ARRAY.decode("{1,NULL,3}"));
    }

    @Test
    public void testArrayColumnType() {
        RDBSchemaMetadata schema = createSchema();
        RDBTableMetadata table = schema.newTable("test_array");
        schema.addTable(table);

        RDBColumnMetadata column = table.newColumn();
        column.setName("tags");
        column.setOwner(table);
        column.setType(table.getDialect().convertDataType("smallint[]"));
        table.addColumn(column);

        Assert.assertEquals("smallint[]", column.getDataType());
        Assert.assertEquals(Short[].class, column.getJavaType());
    }

    private RDBSchemaMetadata createSchema() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.POSTGRES);
        RDBSchemaMetadata schema = new PostgresqlSchemaMetadata("public");
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        return schema;
    }
}
