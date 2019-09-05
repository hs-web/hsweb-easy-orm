package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.metadata.dialect.DefaultDialect;
import org.hswebframework.utils.StringUtils;

import java.sql.JDBCType;

public class MysqlDialect extends DefaultDialect {
    public MysqlDialect() {
        defaultDataTypeMapper = (meta) -> meta.getJdbcType().getName().toLowerCase();
        addDataTypeMapper(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        addDataTypeMapper(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        addDataTypeMapper(JDBCType.TIMESTAMP, (meta) -> "datetime(6)");

        addDataTypeMapper(JDBCType.TIME, (meta) -> "time");
        addDataTypeMapper(JDBCType.DATE, (meta) -> "date");
        addDataTypeMapper(JDBCType.CLOB, (meta) -> "text");
        addDataTypeMapper(JDBCType.LONGVARBINARY, (meta) -> "longblob");
        addDataTypeMapper(JDBCType.LONGVARCHAR, (meta) -> "longtext");
        addDataTypeMapper(JDBCType.BLOB, (meta) -> "blob");
        addDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeMapper(JDBCType.DOUBLE, (meta) -> "double");
        addDataTypeMapper(JDBCType.INTEGER, (meta) -> "int");
        addDataTypeMapper(JDBCType.NUMERIC, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        addDataTypeMapper(JDBCType.DECIMAL, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        addDataTypeMapper(JDBCType.TINYINT, (meta) -> "tinyint");
        addDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeMapper(JDBCType.OTHER, (meta) -> "other");

        addJdbcTypeMapping("int", JDBCType.INTEGER);
        addJdbcTypeMapping("year", JDBCType.TIME);
        addJdbcTypeMapping("datetime", JDBCType.TIMESTAMP);
        addJdbcTypeMapping("text", JDBCType.CLOB);
    }

    @Override
    public String getQuoteStart() {
        return "`";
    }

    @Override
    public String getQuoteEnd() {
        return "`";
    }

    @Override
    public boolean isColumnToUpperCase() {
        return false;
    }

    @Override
    public String getId() {
        return "mysql";
    }

    @Override
    public String getName() {
        return "Mysql";
    }
}
