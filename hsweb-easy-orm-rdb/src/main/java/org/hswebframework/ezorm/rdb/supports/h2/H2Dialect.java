package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.metadata.dialect.DefaultDialect;
import org.hswebframework.utils.StringUtils;

import java.sql.JDBCType;

public class H2Dialect extends DefaultDialect {
    public H2Dialect() {
        defaultDataTypeMapper = (meta) -> meta.getJdbcType().getName().toLowerCase();
        addDataTypeMapper(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        addDataTypeMapper(JDBCType.TIMESTAMP, (meta) -> "timestamp");
        addDataTypeMapper(JDBCType.SMALLINT, (meta) -> "smallint");
        addDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeMapper(JDBCType.TIME, (meta) -> "timestamp");
        addDataTypeMapper(JDBCType.DATE, (meta) -> "date");
        addDataTypeMapper(JDBCType.CLOB, (meta) -> "clob");
        addDataTypeMapper(JDBCType.BLOB, (meta) -> "blob");
        addDataTypeMapper(JDBCType.INTEGER, (meta) -> "int");
        addDataTypeMapper(JDBCType.NUMERIC, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        addDataTypeMapper(JDBCType.TINYINT, (meta) -> "tinyint");
        addDataTypeMapper(JDBCType.DECIMAL, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        addDataTypeMapper(JDBCType.OTHER, (meta) -> "other");
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
        return "h2database";
    }

    @Override
    public String getName() {
        return "H2";
    }
}
