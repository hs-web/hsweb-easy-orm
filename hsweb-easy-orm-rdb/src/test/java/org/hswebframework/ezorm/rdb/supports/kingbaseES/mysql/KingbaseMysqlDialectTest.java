package org.hswebframework.ezorm.rdb.supports.kingbaseES.mysql;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

/**
 * Verifies the public SQL type mapping exposed for KingbaseES MySQL mode.
 */
public class KingbaseMysqlDialectTest {

    // Assertion contracts from upstream metadata/dialect/DialectTest at 96b61b0b.
    @Test
    public void testMysqlEnumDataType() {
        KingbaseMysqlSchemaMetadata schema = new KingbaseMysqlSchemaMetadata("test");
        RDBColumnMetadata column = schema.newTable("test").newColumn();
        column.setName("status");
        column.setType(new KingbaseMysqlDialect().convertDataType("enum('Day','Month')"));

        Assert.assertEquals("enum('Day','Month')", column.getDataType());
        Assert.assertEquals(JDBCType.VARCHAR, column.getSqlType());
        Assert.assertEquals(String.class, column.getJavaType());
        Assert.assertFalse(column.getType().isLengthSupport());
    }

    @Test
    public void testMysqlNumericDataTypeCaseInsensitive() {
        KingbaseMysqlSchemaMetadata schema = new KingbaseMysqlSchemaMetadata("test");
        RDBColumnMetadata column = schema.newTable("test").newColumn();
        column.setName("amount");
        column.setType(new KingbaseMysqlDialect().convertDataType("DECIMAL(10,2)"));

        Assert.assertEquals("decimal(10,2)", column.getDataType());
        Assert.assertEquals(10, column.getPrecision());
        Assert.assertEquals(2, column.getScale());
        Assert.assertEquals(JDBCType.DECIMAL, column.getSqlType());
    }

    @Test
    public void shouldExposeKingbaseMysqlDialectAndVarcharMapping() {
        KingbaseMysqlDialect dialect = new KingbaseMysqlDialect();

        System.out.println("[方言信息] 标识=" + dialect.getId() + "，名称=" + dialect.getName());
        Assert.assertEquals("kingbase-mysql", dialect.getId());
        Assert.assertEquals("KingbaseMysql", dialect.getName());
        String quotedName = dialect.quote("account");
        System.out.println("[标识符引用] 输入=account，实际=" + quotedName);
        Assert.assertEquals("`account`", quotedName);

        RDBColumnMetadata column = new RDBColumnMetadata();
        DataType dataType = dialect.convertDataType("character varying(64)");
        Assert.assertNotNull(dataType);
        Assert.assertEquals(JDBCType.VARCHAR, dataType.getSqlType());
        Assert.assertEquals(String.class, dataType.getJavaType());
        column.setType(dataType);
        Assert.assertEquals(64, column.getLength());
        System.out.println("[数据库类型识别] 输入=character varying(64)，SQL类型="
            + dataType.getSqlType() + "，Java类型=" + dataType.getJavaType().getName());

        String columnType = dialect.buildColumnDataType(column);
        System.out.println("[字段类型生成] 输入长度=64，实际=" + columnType);
        Assert.assertEquals("varchar(64)", columnType);
    }

    @Test
    public void shouldUseRegisteredTypeForNonNumericArguments() {
        KingbaseMysqlDialect dialect = new KingbaseMysqlDialect();
        DataType type = dialect.convertDataType("character varying(abc)");
        Assert.assertEquals(JDBCType.VARCHAR, type.getSqlType());
        Assert.assertEquals(String.class, type.getJavaType());
        System.out.println("[类型参数] 非数字参数按官方参数化类型规则处理，仍识别 character varying");
    }

    @Test
    public void shouldRejectOverflowingTypeLength() {
        KingbaseMysqlDialect dialect = new KingbaseMysqlDialect();
        String length = "999999999999999999999999";
        NumberFormatException exception = Assert.assertThrows(
            NumberFormatException.class,
            () -> dialect.convertDataType("character varying(" + length + ")")
        );
        Assert.assertTrue(exception.getMessage().contains(length));
        System.out.println("[类型转换异常] 超出整数范围的长度触发 NumberFormatException");
    }
}
