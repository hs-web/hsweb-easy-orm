package org.hsweb.ezorm.rdb.render.dialect;

import org.hsweb.commons.StringUtils;

import java.sql.JDBCType;

public class OracleDialect extends DefaultDialect {
    protected OracleDialect() {
        defaultDataTypeMapper = (meta) -> meta.getJdbcType().getName().toLowerCase();
        setDataTypeMapper(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar2(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.TIMESTAMP, (meta) -> "timestamp");
        setDataTypeMapper(JDBCType.TIME, (meta) -> "datetime");
        setDataTypeMapper(JDBCType.DATE, (meta) -> "date");
        setDataTypeMapper(JDBCType.TINYINT, (meta) -> "number(2)");
        setDataTypeMapper(JDBCType.SMALLINT, (meta) -> "number(5)");
        setDataTypeMapper(JDBCType.INTEGER, (meta) -> "integer");
        setDataTypeMapper(JDBCType.DOUBLE, (meta) -> "double");
        setDataTypeMapper(JDBCType.CLOB, (meta) -> "clob");
        setDataTypeMapper(JDBCType.BLOB, (meta) -> "blob");
        setDataTypeMapper(JDBCType.BINARY, (meta) -> "blob");
        setDataTypeMapper(JDBCType.NUMERIC, (meta) -> StringUtils.concat("number(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.DECIMAL, (meta) -> StringUtils.concat("number(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
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
        return new StringBuilder()
                .append("SELECT * FROM ( SELECT row_.*, rownum rownum_ FROM (")
                .append(sql)
                .append(") row_ )")
                .append("WHERE rownum_ <= ")
                .append(pageSize * (pageIndex + 1))
                .append(" AND rownum_ > ")
                .append(pageSize * pageIndex).toString();
    }

    @Override
    public boolean columnToUpperCase() {
        return true;
    }
}
