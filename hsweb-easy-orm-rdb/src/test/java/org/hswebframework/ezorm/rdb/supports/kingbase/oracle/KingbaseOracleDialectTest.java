package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

/**
 * Verifies the public SQL type mapping exposed for KingbaseES Oracle mode.
 */
public class KingbaseOracleDialectTest {

    @Test
    public void shouldMapLobAndCharCatalogAliases() {
        KingbaseOracleDialect dialect = new KingbaseOracleDialect();
        DataType clob = dialect.convertDataType("text");
        Assert.assertEquals(JDBCType.LONGVARCHAR, clob.getSqlType());
        Assert.assertEquals(String.class, clob.getJavaType());

        DataType blob = dialect.convertDataType("bytea");
        Assert.assertEquals(JDBCType.LONGVARBINARY, blob.getSqlType());
        Assert.assertEquals(byte[].class, blob.getJavaType());

        DataType character = dialect.convertDataType("bpchar");
        Assert.assertEquals(JDBCType.CHAR, character.getSqlType());
        Assert.assertEquals(String.class, character.getJavaType());
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setLength(32);
        column.setType(character);
        Assert.assertEquals("char(32)", dialect.buildColumnDataType(column));
        System.out.println("[验证通过] Oracle 系统目录类型 text、bytea、bpchar 分别识别为 CLOB、BLOB、CHAR 对应类型");
    }

    @Test
    public void shouldExposeKingbaseOracleDialectAndCharacterVarchar2Mapping() {
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

        DataType metadataType = dialect.convertDataType("character varying");
        System.out.println("[2. 数据库类型识别] 接口：convertDataType(String dataType)");
        System.out.println("输入：character varying");
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
