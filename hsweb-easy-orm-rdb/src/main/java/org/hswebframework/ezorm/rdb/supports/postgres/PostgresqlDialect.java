package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.dialect.DefaultDialect;
import org.hswebframework.ezorm.core.utils.StringUtils;

import java.math.BigDecimal;
import java.sql.JDBCType;

/**
 * @author zhouhao
 * @since 3.0
 */
public class PostgresqlDialect extends DefaultDialect {


    public PostgresqlDialect() {

        addDataTypeBuilder(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.TIMESTAMP, (meta) -> "timestamp");
        addDataTypeBuilder(JDBCType.TIME, (meta) -> "time");
        addDataTypeBuilder(JDBCType.DATE, (meta) -> "date");
        addDataTypeBuilder(JDBCType.CLOB, (meta) -> "text");
        addDataTypeBuilder(JDBCType.LONGVARBINARY, (meta) -> "bytea");
        addDataTypeBuilder(JDBCType.LONGVARCHAR, (meta) -> "text");
        addDataTypeBuilder(JDBCType.BLOB, (meta) -> "bytea");
        addDataTypeBuilder(JDBCType.DOUBLE, (meta) -> "float8");
        addDataTypeBuilder(JDBCType.FLOAT, (meta) -> "float4");
        addDataTypeBuilder(JDBCType.INTEGER, (meta) -> "int4");
        addDataTypeBuilder(JDBCType.NUMERIC, (meta) -> StringUtils.concat("numeric(", meta.getPrecision(32), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.DECIMAL, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(32), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.TINYINT, (meta) -> "int2");
        addDataTypeBuilder(JDBCType.SMALLINT, (meta) -> "int2");

        addDataTypeBuilder(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeBuilder(JDBCType.OTHER, (meta) -> "other");
        addDataTypeBuilder("json", meta -> "json");
        addDataTypeBuilder("jsonb", meta -> "jsonb");


        registerDataType("json", JsonType.INSTANCE);
        registerDataType("jsonb", JsonbType.INSTANCE);
        registerDataType("clob", DataType.builder(JdbcDataType.of(JDBCType.LONGVARCHAR, String.class), (c) -> "text"));
        registerDataType("blob", DataType.builder(JdbcDataType.of(JDBCType.BLOB, String.class), (c) -> "bytea"));
        registerDataType("longnvarchar", DataType.builder(JdbcDataType.of(JDBCType.LONGNVARCHAR, String.class), c -> "text"));
        registerDataType("longvarchar", DataType.builder(JdbcDataType.of(JDBCType.LONGVARCHAR, String.class), c -> "text"));

        registerDataType("int8", JdbcDataType.of(JDBCType.BIGINT, Long.class));
        registerDataType("int4", JdbcDataType.of(JDBCType.INTEGER, Integer.class));
        registerDataType("int2", JdbcDataType.of(JDBCType.SMALLINT, Byte.class));
        registerDataType("int", JdbcDataType.of(JDBCType.INTEGER, Integer.class));
        registerDataType("float8", JdbcDataType.of(JDBCType.DOUBLE, Double.class));
        registerDataType("float4", JdbcDataType.of(JDBCType.FLOAT, Float.class));
        registerDataType("money", JdbcDataType.of(JDBCType.DECIMAL, BigDecimal.class));
        registerDataType("bool", JdbcDataType.of(JDBCType.BOOLEAN, Boolean.class));
        registerDataType("character", JdbcDataType.of(JDBCType.VARCHAR, String.class));
        registerDataType("xml", JdbcDataType.of(JDBCType.VARCHAR, String.class));
        registerDataType("year", JdbcDataType.of(JDBCType.TIME, String.class));
        registerDataType("datetime", JdbcDataType.of(JDBCType.TIMESTAMP, String.class));
        registerDataType("text", JdbcDataType.of(JDBCType.LONGVARCHAR, String.class));

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
        return false;
    }

    @Override
    public String getId() {
        return "PostgreSQL";
    }

    @Override
    public String getName() {
        return "PostgreSQL";
    }
}
