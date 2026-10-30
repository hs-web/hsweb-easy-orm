package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleDialect;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

/**
 * Verifies the public SQL type mapping exposed for KingbaseES Oracle mode.
 */
public class KingbaseOracleDialectTest {

    @Test
    public void shouldReuseOracleTypeMappingsAndCustomTypeFallback() {
        OracleDialect original = new OracleDialect();
        KingbaseOracleDialect kingbase = new KingbaseOracleDialect();
        for (String name : new String[]{"smallint", "integer", "bigint", "real", "numeric",
            "varchar", "varchar2", "char", "character varying", "character", "timestamp", "time", "json", "unknown_type"}) {
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
            System.out.println("[类型映射对比] Oracle/Kingbase Oracle，类型=" + name
                + "，JDBC=" + actual.getSqlType() + "，Java=" + actual.getJavaType().getName()
                + "，DDL=" + expectedSql);
        }
    }

    @Test
    public void shouldReuseOracleBooleanMapping() {
        OracleDialect original = new OracleDialect();
        KingbaseOracleDialect kingbase = new KingbaseOracleDialect();
        Assert.assertEquals(JDBCType.TINYINT, original.convertSqlType(Boolean.class).get());
        Assert.assertEquals(JDBCType.TINYINT, kingbase.convertSqlType(Boolean.class).get());
        Assert.assertEquals(JDBCType.BOOLEAN, kingbase.convertSqlType(boolean.class).get());
        DataType expected = original.convertDataType("boolean");
        DataType actual = kingbase.convertDataType("boolean");
        Assert.assertEquals(expected.getSqlType(), actual.getSqlType());
        Assert.assertEquals(expected.getJavaType(), actual.getJavaType());
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setType(actual);
        Assert.assertEquals("number(1)", original.buildColumnDataType(column));
        Assert.assertEquals("number(1)", kingbase.buildColumnDataType(column));
        column.setJdbcType(kingbase.convertSqlType(Boolean.class).get(), Boolean.class);
        Assert.assertEquals("number(1)", kingbase.buildColumnDataType(column));
        System.out.println("[Boolean 映射] 沿用 Oracle：Boolean 推导为 TINYINT，boolean 推导为 BOOLEAN，两者均生成 number(1)");
    }

    @Test
    public void shouldUseOracleCustomTypeFallbackForCatalogNames() {
        KingbaseOracleDialect dialect = new KingbaseOracleDialect();
        OracleDialect original = new OracleDialect();
        for (String name : new String[]{"text", "bytea", "bpchar", "double precision",
            "timestamp without time zone", "time without time zone"}) {
            DataType actual = dialect.convertDataType(name);
            DataType expected = original.convertDataType(name);
            Assert.assertEquals(name, JDBCType.OTHER, actual.getSqlType());
            Assert.assertEquals(name, String.class, actual.getJavaType());
            Assert.assertEquals(name, expected.getSqlType(), actual.getSqlType());
            Assert.assertEquals(name, expected.getJavaType(), actual.getJavaType());
            Assert.assertEquals(name, name, actual.getName());
            RDBColumnMetadata column = new RDBColumnMetadata();
            column.setLength(32);
            column.setType(actual);
            Assert.assertEquals(name, name, dialect.buildColumnDataType(column));
            System.out.println("[原生回退规则] Oracle/Kingbase Oracle，名称=" + name + "，JDBC=OTHER，Java=String");
        }
    }

    @Test
    public void shouldExposeKingbaseOracleDialectAndVarchar2Mapping() {
        KingbaseOracleDialect dialect = new KingbaseOracleDialect();

        System.out.println("[方言信息] 标识=" + dialect.getId() + "，名称=" + dialect.getName());
        System.out.println("[大小写策略] 预期=true，实际=" + dialect.isColumnToUpperCase());
        Assert.assertEquals("kingbase-oracle", dialect.getId());
        Assert.assertEquals("KingbaseOracle", dialect.getName());
        Assert.assertTrue(dialect.isColumnToUpperCase());

        String quotedName = dialect.quote("account");
        System.out.println("[1. 标识符引用] 接口：quote(String keyword)");
        System.out.println("输入：account");
        System.out.println("预期：\"ACCOUNT\"，实际：" + quotedName);
        Assert.assertEquals("\"ACCOUNT\"", quotedName);

        DataType metadataType = dialect.convertDataType("varchar");
        System.out.println("[2. 数据库类型识别] 接口：convertDataType(String dataType)");
        System.out.println("输入：varchar");
        System.out.println("预期：SQL类型=VARCHAR，Java类型=java.lang.String");
        Assert.assertNotNull(metadataType);
        System.out.println("实际：SQL类型=" + metadataType.getSqlType()
            + "，Java类型=" + metadataType.getJavaType().getName());
        Assert.assertEquals(JDBCType.VARCHAR, metadataType.getSqlType());
        Assert.assertEquals(String.class, metadataType.getJavaType());

        DataType varchar2Type = dialect.convertDataType("varchar2");
        Assert.assertNotNull(varchar2Type);
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setLength(64);
        column.setType(varchar2Type);
        String columnType = dialect.buildColumnDataType(column);
        System.out.println("[3. 字段类型生成] 接口：buildColumnDataType(RDBColumnMetadata columnMetaData)");
        System.out.println("输入：类型=varchar2，长度=" + column.getLength());
        System.out.println("预期：varchar2(64 char)，实际：" + columnType);
        Assert.assertEquals("varchar2(64 char)", columnType);
    }
}
