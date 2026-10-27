package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleDialect;

import java.sql.JDBCType;

/**
 * SQL dialect for KingbaseES running in Oracle compatibility mode.
 *
 * <p>The base Oracle dialect provides Oracle SQL rendering, including
 * {@code varchar2(n char)}. This dialect adds the type names returned by
 * KingbaseES metadata.</p>
 */
public class KingbaseOracleDialect extends OracleDialect {

    public KingbaseOracleDialect() {
        registerDataType("character varying", DataType.builder(
            JdbcDataType.of(JDBCType.VARCHAR, String.class),
            column -> "varchar(" + column.getLength(255) + ")"
        ));
        registerDataType("character", DataType.builder(
            JdbcDataType.of(JDBCType.CHAR, String.class),
            column -> "char(" + column.getLength(255) + ")"
        ));
        // KES 系统目录使用底层类型名返回 Oracle CLOB、BLOB 和 CHAR。
        registerDataType("text", JdbcDataType.of(JDBCType.LONGVARCHAR, String.class));
        registerDataType("bytea", JdbcDataType.of(JDBCType.LONGVARBINARY, byte[].class));
        registerDataType("bpchar", DataType.builder(
            JdbcDataType.of(JDBCType.CHAR, String.class),
            column -> "char(" + column.getLength(255) + ")"
        ));
        registerDataType("integer", JdbcDataType.of(JDBCType.INTEGER, Integer.class));
        registerDataType("smallint", JdbcDataType.of(JDBCType.SMALLINT, Short.class));
        registerDataType("bigint", JdbcDataType.of(JDBCType.BIGINT, Long.class));
        registerDataType("boolean", JdbcDataType.of(JDBCType.BOOLEAN, Boolean.class));
        registerDataType("double precision", JdbcDataType.of(JDBCType.DOUBLE, Double.class));
        registerDataType("real", JdbcDataType.of(JDBCType.FLOAT, Float.class));
        registerDataType("timestamp without time zone", JdbcDataType.of(JDBCType.TIMESTAMP, java.sql.Timestamp.class));
        registerDataType("time without time zone", JdbcDataType.of(JDBCType.TIME, java.sql.Time.class));
        registerDataType("numeric", JdbcDataType.of(JDBCType.NUMERIC, java.math.BigDecimal.class));
        // Kingbase Oracle 兼容模式原生支持 boolean 类型，恢复基类 OracleDialect 的
        // Boolean->TINYINT(number(1)) 映射，否则 NVL(boolean 绑定, numeric 列) 等表达式会因类型不一致报错
        classJDBCTypeMapping.put(Boolean.class, JDBCType.BOOLEAN);
        classJDBCTypeMapping.put(boolean.class, JDBCType.BOOLEAN);
        addDataTypeBuilder(JDBCType.BOOLEAN, column -> "boolean");
    }

    @Override
    public String getId() {
        return "kingbase-oracle";
    }

    @Override
    public String getName() {
        return "KingbaseOracle";
    }
}
