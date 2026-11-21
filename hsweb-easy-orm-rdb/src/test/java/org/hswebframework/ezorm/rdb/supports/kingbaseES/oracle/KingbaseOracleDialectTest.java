package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

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
    public void shouldRecognizeCatalogQualifiedVarchar() {
        KingbaseOracleDialect dialect = new KingbaseOracleDialect();
        for (String name : new String[]{"pg_catalog.varchar", " PG_CATALOG.VARCHAR(64) ",
            "varchar", " VARCHAR(64) ", "varchar2", "VARCHAR2(64)"}) {
            DataType type = dialect.convertDataType(name);
            Assert.assertEquals(name, JDBCType.VARCHAR, type.getSqlType());
            Assert.assertEquals(name, String.class, type.getJavaType());
            RDBColumnMetadata column = new RDBColumnMetadata();
            column.setType(type);
            if (!name.contains("(")) {
                column.setLength(64);
            }
            Assert.assertEquals(name, 64, column.getLength());
            // VARCHAR2 rendering contract from upstream OracleTableMetaParserTest.testParse.
            Assert.assertEquals(name, "varchar2(64 char)", dialect.buildColumnDataType(column));
        }
        DataType custom = dialect.convertDataType("business.varchar");
        Assert.assertEquals(JDBCType.OTHER, custom.getSqlType());
        Assert.assertEquals("business.varchar", custom.getName());
    }

    // Fixed contracts from upstream OracleTableMetaParserTest.testParse at 96b61b0b.
    @Test
    public void shouldMatchOfficialOracleNumberAndVarchar2Contracts() {
        KingbaseOracleDialect dialect = new KingbaseOracleDialect();
        Object[][] cases = {
            {"varchar2(32)", JDBCType.VARCHAR, String.class, "varchar2(32 char)"},
            {"varchar2(128)", JDBCType.VARCHAR, String.class, "varchar2(128 char)"},
            {"number(10,0)", JDBCType.NUMERIC, java.math.BigDecimal.class, "number(10,0)"},
            {"numeric(10,0)", JDBCType.NUMERIC, java.math.BigDecimal.class, "number(10,0)"},
            {" NUMERIC(12,2) ", JDBCType.NUMERIC, java.math.BigDecimal.class, "number(12,2)"}
        };
        for (Object[] testCase : cases) {
            RDBColumnMetadata column = new RDBColumnMetadata();
            column.setType(dialect.convertDataType((String) testCase[0]));
            Assert.assertEquals(testCase[0].toString(), testCase[1], column.getSqlType());
            Assert.assertEquals(testCase[0].toString(), testCase[2], column.getJavaType());
            Assert.assertEquals(testCase[0].toString(), testCase[3], dialect.buildColumnDataType(column));
        }
    }

    @Test
    public void shouldReuseOracleBooleanMapping() {
        KingbaseOracleDialect kingbase = new KingbaseOracleDialect();
        Assert.assertEquals(JDBCType.TINYINT, kingbase.convertSqlType(Boolean.class).get());
        Assert.assertEquals(JDBCType.BOOLEAN, kingbase.convertSqlType(boolean.class).get());
        DataType actual = kingbase.convertDataType("boolean");
        Assert.assertEquals(JDBCType.BOOLEAN, actual.getSqlType());
        Assert.assertEquals(String.class, actual.getJavaType());
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setType(actual);
        Assert.assertEquals("number(1)", kingbase.buildColumnDataType(column));
        column.setJdbcType(kingbase.convertSqlType(Boolean.class).get(), Boolean.class);
        Assert.assertEquals("number(1)", kingbase.buildColumnDataType(column));
        System.out.println("[Boolean 映射] 沿用 Oracle：Boolean 推导为 TINYINT，boolean 推导为 BOOLEAN，两者均生成 number(1)");
    }

    @Test
    public void shouldReuseOracleMappingsForObservedDatabaseTypeNames() {
        KingbaseOracleDialect dialect = new KingbaseOracleDialect();
        // Compatibility supplements use fixed contracts, not the implementation as an oracle.
        Object[][] mappings = {
            {"pg_catalog.varchar", JDBCType.VARCHAR, String.class, "varchar2(32 char)"},
            {"timestamp without time zone", JDBCType.TIMESTAMP, java.sql.Timestamp.class, "timestamp"},
            {"text", JDBCType.LONGVARCHAR, String.class, "clob"},
            {"bytea", JDBCType.LONGVARBINARY, byte[].class, "blob"},
            // Upstream DefaultDialect registers the type name "double" with its own builder.
            // OracleDialect's "binary_double" builder applies to JDBC DOUBLE, a separate path.
            {"double precision", JDBCType.DOUBLE, Double.class, "double"}
        };
        for (Object[] mapping : mappings) {
            DataType actual = dialect.convertDataType((String) mapping[0]);
            Assert.assertEquals(mapping[0].toString(), mapping[1], actual.getSqlType());
            Assert.assertEquals(mapping[0].toString(), mapping[2], actual.getJavaType());
            RDBColumnMetadata column = new RDBColumnMetadata();
            column.setLength(32);
            column.setType(actual);
            Assert.assertEquals(mapping[0].toString(), mapping[3], dialect.buildColumnDataType(column));
        }
    }

    @Test
    public void shouldPreserveUnknownAndUserDefinedTypes() {
        KingbaseOracleDialect dialect = new KingbaseOracleDialect();
        for (String name : new String[]{"bpchar", "time without time zone", "business.varchar",
            "business.numeric", "business.numeric(10,0)", "unknown_type"}) {
            DataType actual = dialect.convertDataType(name);
            Assert.assertEquals(name, JDBCType.OTHER, actual.getSqlType());
            Assert.assertEquals(name, String.class, actual.getJavaType());
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
