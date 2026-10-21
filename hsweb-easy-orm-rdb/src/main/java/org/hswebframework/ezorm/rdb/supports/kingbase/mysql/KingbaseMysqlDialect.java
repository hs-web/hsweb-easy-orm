package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlDialect;

import java.sql.JDBCType;

/**
 * MySQL-compatible SQL rendering and type mappings for KingbaseES.
 */
public class KingbaseMysqlDialect extends MysqlDialect {

    public KingbaseMysqlDialect() {
        registerDataType("character varying", DataType.builder(JdbcDataType.of(JDBCType.VARCHAR, String.class),
                                                               column -> "varchar(" + column.getLength(255) + ")"));
        registerDataType("character", DataType.builder(JdbcDataType.of(JDBCType.CHAR, String.class),
                                                       column -> "char(" + column.getLength(255) + ")"));
        registerDataType("integer", JdbcDataType.of(JDBCType.INTEGER, Integer.class));
        registerDataType("smallint", JdbcDataType.of(JDBCType.SMALLINT, Short.class));
        registerDataType("bigint", JdbcDataType.of(JDBCType.BIGINT, Long.class));
        registerDataType("boolean", JdbcDataType.of(JDBCType.BOOLEAN, Boolean.class));
        registerDataType("double precision", JdbcDataType.of(JDBCType.DOUBLE, Double.class));
        registerDataType("real", JdbcDataType.of(JDBCType.FLOAT, Float.class));
        registerDataType("timestamp without time zone", JdbcDataType.of(JDBCType.TIMESTAMP, java.sql.Timestamp.class));
        registerDataType("time without time zone", JdbcDataType.of(JDBCType.TIME, java.sql.Time.class));
    }
    @Override
    public String getId() {
        return "kingbase-mysql";
    }

    @Override
    public String getName() {
        return "KingbaseMysql";
    }
}
