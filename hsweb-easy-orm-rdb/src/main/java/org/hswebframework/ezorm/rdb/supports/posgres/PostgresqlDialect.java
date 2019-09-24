package org.hswebframework.ezorm.rdb.supports.posgres;

import org.hswebframework.ezorm.rdb.metadata.dialect.DefaultDialect;
import org.hswebframework.utils.StringUtils;

import java.sql.JDBCType;

/**
 * @author zhouhao
 * @since 3.0
 */
public class PostgresqlDialect extends DefaultDialect {


    public PostgresqlDialect() {

        addDataTypeMapper(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        addDataTypeMapper(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        addDataTypeMapper(JDBCType.TIMESTAMP, (meta) -> "timestamp");
        addDataTypeMapper(JDBCType.TIME, (meta) -> "time");
        addDataTypeMapper(JDBCType.DATE, (meta) -> "date");
        addDataTypeMapper(JDBCType.CLOB, (meta) -> "text");
        addDataTypeMapper(JDBCType.LONGVARBINARY, (meta) -> "bytea");
        addDataTypeMapper(JDBCType.LONGVARCHAR, (meta) -> "text");
        addDataTypeMapper(JDBCType.BLOB, (meta) -> "bytea");
        addDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeMapper(JDBCType.DOUBLE, (meta) -> "double");
        addDataTypeMapper(JDBCType.INTEGER, (meta) -> "integer");
        addDataTypeMapper(JDBCType.NUMERIC, (meta) -> StringUtils.concat("numeric(", meta.getPrecision(), ",", meta.getScale(), ")"));
        addDataTypeMapper(JDBCType.DECIMAL, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        addDataTypeMapper(JDBCType.TINYINT, (meta) -> "smallint");
        addDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeMapper(JDBCType.OTHER, (meta) -> "other");

        dataTypeMappers.put("json",meta->"json");
        dataTypeMappers.put("jsonb",meta->"jsonb");

        addJdbcTypeMapping("int", JDBCType.INTEGER);
        addJdbcTypeMapping("int2", JDBCType.INTEGER);
        addJdbcTypeMapping("int4", JDBCType.INTEGER);

        addJdbcTypeMapping("float8", JDBCType.DOUBLE);
        addJdbcTypeMapping("money", JDBCType.DOUBLE);
        addJdbcTypeMapping("bool", JDBCType.BOOLEAN);

        addJdbcTypeMapping("character", JDBCType.VARCHAR);
        addJdbcTypeMapping("xml", JDBCType.VARCHAR);

        addJdbcTypeMapping("year", JDBCType.TIME);
        addJdbcTypeMapping("datetime", JDBCType.TIMESTAMP);
        addJdbcTypeMapping("text", JDBCType.CLOB);


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
