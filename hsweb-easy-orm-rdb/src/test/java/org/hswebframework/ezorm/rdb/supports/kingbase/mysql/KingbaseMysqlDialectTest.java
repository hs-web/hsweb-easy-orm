package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlDialect;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

/**
 * Verifies the public SQL type mapping exposed for KingbaseES MySQL mode.
 */
public class KingbaseMysqlDialectTest {

    @Test
    public void shouldReuseMysqlTypeMappingsAndCustomTypeFallback() {
        MysqlDialect original = new MysqlDialect();
        KingbaseMysqlDialect kingbase = new KingbaseMysqlDialect();
        for (String name : new String[]{"smallint", "integer", "bigint", "boolean", "real", "numeric",
            "varchar", "varchar(64)", "character", "character(32)",
            "double precision", "timestamp", "datetime", "time", "timestamp without time zone", "time without time zone", "json", "unknown_type"}) {
            DataType expected = original.convertDataType(name);
            DataType actual = kingbase.convertDataType(name);
            Assert.assertEquals(name, expected.getSqlType(), actual.getSqlType());
            Assert.assertEquals(name, expected.getJavaType(), actual.getJavaType());
            RDBColumnMetadata column = new RDBColumnMetadata();
            column.setPrecision(12);
            column.setScale(2);
            column.setType(expected);
            String expectedSql = original.buildColumnDataType(column);
            column.setType(actual);
            Assert.assertEquals(name, expectedSql, kingbase.buildColumnDataType(column));
            System.out.println("[类型映射对比] MySQL/Kingbase MySQL，类型=" + name
                + "，JDBC=" + actual.getSqlType() + "，Java=" + actual.getJavaType().getName()
                + "，DDL=" + expectedSql);
        }
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
    public void shouldRejectInvalidTypeLength() {
        KingbaseMysqlDialect dialect = new KingbaseMysqlDialect();
        String typeName = "character varying(abc)";
        System.out.println("[类型转换异常] 输入=" + typeName + "，长度参数应为整数");

        NumberFormatException exception = Assert.assertThrows(
            NumberFormatException.class,
            () -> dialect.convertDataType(typeName)
        );
        System.out.println("[实际异常] " + exception.getClass().getName() + ": " + exception.getMessage());
        Assert.assertTrue(exception.getMessage().contains("abc"));
        System.out.println("[验证结果] 非法长度参数触发 NumberFormatException，测试通过");
    }
}
