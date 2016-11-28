package org.hsweb.ezorm.rdb.render.dialect;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.parser.MysqlTableMetaParser;
import org.hsweb.ezorm.rdb.meta.parser.TableMetaParser;

import java.sql.JDBCType;

public class MysqlDialect extends DefaultDialect {
    protected MysqlDialect() {
        defaultDataTypeMapper = (meta) -> meta.getJdbcType().getName().toLowerCase();
        setDataTypeMapper(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.TIMESTAMP, (meta) -> "datetime");
        setDataTypeMapper(JDBCType.TIME, (meta) -> "time");
        setDataTypeMapper(JDBCType.DATE, (meta) -> "year");
        setDataTypeMapper(JDBCType.CLOB, (meta) -> "text");
        setDataTypeMapper(JDBCType.LONGVARBINARY, (meta) -> "longblob");
        setDataTypeMapper(JDBCType.LONGVARCHAR, (meta) -> "longtext");
        setDataTypeMapper(JDBCType.BLOB, (meta) -> "blob");
        setDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        setDataTypeMapper(JDBCType.DOUBLE, (meta) -> "double");
        setDataTypeMapper(JDBCType.INTEGER, (meta) -> "int");
        setDataTypeMapper(JDBCType.NUMERIC, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.DECIMAL, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.TINYINT, (meta) -> "tinyint");
        setDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        setDataTypeMapper(JDBCType.OTHER, (meta) -> "other");
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
    public String doPaging(String sql, int pageIndex, int pageSize) {
        return new StringBuilder(sql)
                .append(" limit ")
                .append(pageSize * pageIndex)
                .append(",")
                .append(pageSize).toString();
    }

    @Override
    public boolean columnToUpperCase() {
        return false;
    }

    @Override
    public TableMetaParser getDefaultParser(SqlExecutor sqlExecutor) {
        return new MysqlTableMetaParser(sqlExecutor);
    }
}
