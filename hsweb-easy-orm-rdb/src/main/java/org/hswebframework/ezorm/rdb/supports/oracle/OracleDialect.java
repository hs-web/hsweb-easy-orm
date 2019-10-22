package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.dialect.DefaultDialect;
import org.hswebframework.utils.StringUtils;

import java.sql.Date;
import java.sql.JDBCType;

public class OracleDialect extends DefaultDialect {
    public OracleDialect() {
        super();
        addDataTypeBuilder(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.NCHAR, (meta) -> StringUtils.concat("nchar(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar2(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.NVARCHAR, (meta) -> StringUtils.concat("nvarchar2(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.TIMESTAMP, (meta) -> "timestamp");
        addDataTypeBuilder(JDBCType.TIME, (meta) -> "timestamp");
        addDataTypeBuilder(JDBCType.DATE, (meta) -> "date");
        addDataTypeBuilder(JDBCType.TINYINT, (meta) -> "number(2)");
        addDataTypeBuilder(JDBCType.SMALLINT, (meta) -> "number(5)");
        addDataTypeBuilder(JDBCType.INTEGER, (meta) -> "integer");
        addDataTypeBuilder(JDBCType.DOUBLE, (meta) -> "binary_double");
        addDataTypeBuilder(JDBCType.CLOB, (meta) -> "clob");
        addDataTypeBuilder(JDBCType.LONGNVARCHAR, (meta) -> "clob");
        addDataTypeBuilder(JDBCType.BLOB, (meta) -> "blob");
        addDataTypeBuilder(JDBCType.BINARY, (meta) -> "blob");
        addDataTypeBuilder(JDBCType.NUMERIC, (meta) -> StringUtils.concat("number(", meta.getPrecision(38), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.DECIMAL, (meta) -> StringUtils.concat("number(", meta.getPrecision(38), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.BIGINT, (meta) -> "number(38,0)");
        addDataTypeBuilder(JDBCType.OTHER, (meta) -> "other");

        registerDataType("varchar2", DataType.builder(DataType.jdbc(JDBCType.VARCHAR, String.class),
                column -> "varchar2(" + column.getLength() + ")"));

        registerDataType("nvarchar2", DataType.builder(DataType.jdbc(JDBCType.VARCHAR, String.class),
                column -> "nvarchar2(" + column.getLength() + ")"));

        registerDataType("date", JdbcDataType.of(JDBCType.TIMESTAMP, Date.class));
        registerDataType("clob", JdbcDataType.of(JDBCType.LONGVARCHAR, String.class));

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
