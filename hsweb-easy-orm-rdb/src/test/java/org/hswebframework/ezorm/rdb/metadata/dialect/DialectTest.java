package org.hswebframework.ezorm.rdb.metadata.dialect;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbase.mysql.KingbaseMysqlDialect;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSchemaMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

public class DialectTest {

    @Test
    public void test() {
        Dialect dialect = Dialect.H2;

        Assert.assertEquals(dialect.quote("name"), "\"NAME\"");

        Assert.assertEquals(dialect.clearQuote("\"NAME\""), "NAME");

        Assert.assertEquals(dialect.clearQuote("test.\"NAME\""), "test.NAME");

        Assert.assertEquals(dialect.clearQuote("db1.test.\"NAME\""), "db1.test.NAME");

    }

    @Test
    public void testMysqlEnumDataType() {
        assertMysqlLiteralEnumType(Dialect.MYSQL.convertDataType("enum('Day','Month')"));
        assertMysqlLiteralEnumType(new KingbaseMysqlDialect().convertDataType("enum('Day','Month')"));
    }

    @Test
    public void testMysqlNumericDataTypeCaseInsensitive() {
        MysqlSchemaMetadata schema = new MysqlSchemaMetadata("test");
        RDBTableMetadata table = schema.newTable("test");
        RDBColumnMetadata column = table.newColumn();
        column.setName("amount");
        column.setType(Dialect.MYSQL.convertDataType("DECIMAL(10,2)"));
        table.addColumn(column);

        Assert.assertEquals("decimal(10,2)", column.getDataType());
        Assert.assertEquals(10, column.getPrecision());
        Assert.assertEquals(2, column.getScale());
        Assert.assertEquals(JDBCType.DECIMAL, column.getSqlType());
    }

    private void assertMysqlLiteralEnumType(DataType type) {
        MysqlSchemaMetadata schema = new MysqlSchemaMetadata("test");
        RDBTableMetadata table = schema.newTable("test");
        RDBColumnMetadata column = table.newColumn();
        column.setName("status");
        column.setType(type);
        table.addColumn(column);

        Assert.assertEquals("enum('Day','Month')", column.getDataType());
        Assert.assertEquals(JDBCType.VARCHAR, column.getSqlType());
        Assert.assertEquals(String.class, column.getJavaType());
        Assert.assertFalse(column.getType().isLengthSupport());
    }

}
