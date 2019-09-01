package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.meta.dialect.DefaultDialect;
import org.hswebframework.utils.StringUtils;

import java.sql.JDBCType;

public class OracleDialect extends DefaultDialect {
    public OracleDialect() {
        defaultDataTypeMapper = (meta) -> meta.getJdbcType().getName().toLowerCase();
        addDataTypeMapper(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        addDataTypeMapper(JDBCType.NCHAR, (meta) -> StringUtils.concat("nchar(", meta.getLength(), ")"));
        addDataTypeMapper(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar2(", meta.getLength(), ")"));
        addDataTypeMapper(JDBCType.NVARCHAR, (meta) -> StringUtils.concat("nvarchar2(", meta.getLength(), ")"));
        addDataTypeMapper(JDBCType.TIMESTAMP, (meta) -> "timestamp");
        addDataTypeMapper(JDBCType.TIME, (meta) -> "datetime");
        addDataTypeMapper(JDBCType.DATE, (meta) -> "date");
        addDataTypeMapper(JDBCType.TINYINT, (meta) -> "number(2)");
        addDataTypeMapper(JDBCType.SMALLINT, (meta) -> "number(5)");
        addDataTypeMapper(JDBCType.INTEGER, (meta) -> "integer");
        addDataTypeMapper(JDBCType.DOUBLE, (meta) -> "double");
        addDataTypeMapper(JDBCType.CLOB, (meta) -> "clob");
        addDataTypeMapper(JDBCType.BLOB, (meta) -> "blob");
        addDataTypeMapper(JDBCType.BINARY, (meta) -> "blob");
        addDataTypeMapper(JDBCType.NUMERIC, (meta) -> StringUtils.concat("number(", meta.getPrecision(), ",", meta.getScale(), ")"));
        addDataTypeMapper(JDBCType.DECIMAL, (meta) -> StringUtils.concat("number(", meta.getPrecision(), ",", meta.getScale(), ")"));
        addDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeMapper(JDBCType.OTHER, (meta) -> "other");

        addJdbcTypeMapping("varchar2", JDBCType.VARCHAR);
        addJdbcTypeMapping("number", JDBCType.NUMERIC);
        addJdbcTypeMapping("date", JDBCType.TIMESTAMP);
        addJdbcTypeMapping("nvarchar2", JDBCType.NVARCHAR);
        addJdbcTypeMapping("timestamp", JDBCType.TIMESTAMP);


    }

    @Override
    public String getQuoteStart() {
        return "\"";
    }

    @Override
    public String getQuoteEnd() {
        return "\"";
    }

    @Override
    public boolean isColumnToUpperCase() {
        return true;
    }

    @Override
    public String getId() {
        return "oracle";
    }

    @Override
    public String getName() {
        return "Oracle";
    }
}
