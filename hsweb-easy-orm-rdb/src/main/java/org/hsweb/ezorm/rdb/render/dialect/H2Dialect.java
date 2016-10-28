package org.hsweb.ezorm.rdb.render.dialect;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.parser.H2TableMetaParser;
import org.hsweb.ezorm.rdb.meta.parser.TableMetaParser;

import java.sql.JDBCType;

public class H2Dialect extends DefaultDialect {
    public H2Dialect() {
        setDataTypeMapper(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.TIMESTAMP, (meta) -> "timestamp");
        setDataTypeMapper(JDBCType.TIME, (meta) -> "timestamp");
        setDataTypeMapper(JDBCType.DATE, (meta) -> "date");
        setDataTypeMapper(JDBCType.CLOB, (meta) -> "clob");
        setDataTypeMapper(JDBCType.BLOB, (meta) -> "blob");
        setDataTypeMapper(JDBCType.INTEGER, (meta) -> "int");
        setDataTypeMapper(JDBCType.NUMERIC, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.TINYINT, (meta) -> "tinyint");
        setDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        setDataTypeMapper(JDBCType.DECIMAL, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.OTHER, (meta) -> "other");
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
    public String doPaging(String sql, int pageIndex, int pageSize) {
        return new StringBuilder(sql)
                .append(" limit ")
                .append(pageSize * pageIndex)
                .append(",")
                .append(pageSize * (pageIndex + 1)).toString();
    }

    @Override
    public boolean columnToUpperCase() {
        return true;
    }

    @Override
    public TableMetaParser getDefaultParser(SqlExecutor sqlExecutor) {
        return new H2TableMetaParser(sqlExecutor);
    }
}
