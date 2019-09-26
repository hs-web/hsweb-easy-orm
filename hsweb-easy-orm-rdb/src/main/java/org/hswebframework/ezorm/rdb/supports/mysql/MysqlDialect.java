package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.dialect.DefaultDialect;
import org.hswebframework.utils.StringUtils;

import java.sql.Date;
import java.sql.JDBCType;

public class MysqlDialect extends DefaultDialect {
    public MysqlDialect() {
        super();
        addDataTypeBuilder(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.TIMESTAMP, (meta) -> "datetime(6)");
        addDataTypeBuilder(JDBCType.TIME, (meta) -> "time");
        addDataTypeBuilder(JDBCType.DATE, (meta) -> "date");
        addDataTypeBuilder(JDBCType.CLOB, (meta) -> "text");
        addDataTypeBuilder(JDBCType.LONGVARBINARY, (meta) -> "longblob");
        addDataTypeBuilder(JDBCType.LONGVARCHAR, (meta) -> "longtext");
        addDataTypeBuilder(JDBCType.BLOB, (meta) -> "blob");
        addDataTypeBuilder(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeBuilder(JDBCType.DOUBLE, (meta) -> "double");
        addDataTypeBuilder(JDBCType.INTEGER, (meta) -> "int");
        addDataTypeBuilder(JDBCType.NUMERIC, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.DECIMAL, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.TINYINT, (meta) -> "tinyint");
        addDataTypeBuilder(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeBuilder(JDBCType.OTHER, (meta) -> "other");
        addDataTypeBuilder("int", (meta) -> "int");
        addDataTypeBuilder("json", meta->"json");

        registerDataType("int", JdbcDataType.of(JDBCType.INTEGER,Integer.class));
        registerDataType("text", JdbcDataType.of(JDBCType.CLOB,String.class));
        registerDataType("longtext", JdbcDataType.of(JDBCType.LONGVARCHAR,String.class));
        registerDataType("year", JdbcDataType.of(JDBCType.DATE, Date.class));
        registerDataType("text", JdbcDataType.of(JDBCType.CLOB, Date.class));
        registerDataType("datetime", JdbcDataType.of(JDBCType.TIMESTAMP, Date.class));

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
