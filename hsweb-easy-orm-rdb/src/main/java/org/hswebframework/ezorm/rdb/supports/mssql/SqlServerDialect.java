package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.dialect.DefaultDialect;
import org.hswebframework.utils.StringUtils;

import java.sql.Date;
import java.sql.JDBCType;

/**
 * @author zhouhao
 */
public class SqlServerDialect extends DefaultDialect {

    public SqlServerDialect() {
        super();
        addDataTypeMapper(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        addDataTypeMapper(JDBCType.NCHAR, (meta) -> StringUtils.concat("nchar(", meta.getLength(), ")"));
        addDataTypeMapper(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        addDataTypeMapper(JDBCType.NVARCHAR, (meta) -> StringUtils.concat("nvarchar(", meta.getLength(), ")"));
        addDataTypeMapper(JDBCType.TIMESTAMP, (meta) -> "datetime2");
        addDataTypeMapper(JDBCType.TIME, (meta) -> "time");
        addDataTypeMapper(JDBCType.DATE, (meta) -> "date");
        addDataTypeMapper(JDBCType.CLOB, (meta) -> "text");
        addDataTypeMapper(JDBCType.LONGVARBINARY, (meta) -> "varbinary(max)");
        addDataTypeMapper(JDBCType.LONGVARCHAR, (meta) -> "text");
        addDataTypeMapper(JDBCType.BLOB, (meta) -> "varbinary(max)");
        addDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeMapper(JDBCType.DOUBLE, (meta) -> "double");
        addDataTypeMapper(JDBCType.INTEGER, (meta) -> "int");
        addDataTypeMapper(JDBCType.NUMERIC, (meta) -> StringUtils.concat("numeric(", meta.getPrecision(), ",", meta.getScale(), ")"));
        addDataTypeMapper(JDBCType.DECIMAL, (meta) -> StringUtils.concat("numeric(", meta.getPrecision(), ",", meta.getScale(), ")"));
        addDataTypeMapper(JDBCType.TINYINT, (meta) -> "tinyint");
        addDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeMapper(JDBCType.OTHER, (meta) -> "other");
        addDataTypeMapper(JDBCType.REAL, (meta) -> "real");

        addDataTypeMapper("datetime2", (meta) -> "datetime2");
        addDataTypeMapper("datetime", (meta) -> "datetime");

        registerDataType("int", JdbcDataType.of(JDBCType.INTEGER, Integer.class));

        registerDataType("datetime2", JdbcDataType.of(JDBCType.TIMESTAMP, java.util.Date.class));
        registerDataType("datetime", JdbcDataType.of(JDBCType.TIMESTAMP, java.util.Date.class));
        registerDataType("nvarchar", JdbcDataType.of(JDBCType.NVARCHAR, String.class));

        addJdbcTypeMapping("bigint", JDBCType.BIGINT);
        addJdbcTypeMapping("binary", JDBCType.BINARY);
        addJdbcTypeMapping("bit", JDBCType.BIT);
        addJdbcTypeMapping("char", JDBCType.CHAR);
        addJdbcTypeMapping("datetime", JDBCType.TIMESTAMP);
        addJdbcTypeMapping("datetime2", JDBCType.TIMESTAMP);
        addJdbcTypeMapping("decimal", JDBCType.DECIMAL);
        addJdbcTypeMapping("float", JDBCType.FLOAT);
        addJdbcTypeMapping("image", JDBCType.LONGVARBINARY);
        addJdbcTypeMapping("int", JDBCType.INTEGER);
        addJdbcTypeMapping("money", JDBCType.DECIMAL);
        addJdbcTypeMapping("nchar", JDBCType.CHAR);
        addJdbcTypeMapping("ntext", JDBCType.LONGVARCHAR);
        addJdbcTypeMapping("numeric", JDBCType.NUMERIC);
        addJdbcTypeMapping("nvarchar", JDBCType.VARCHAR);
        addJdbcTypeMapping("real", JDBCType.REAL);
        addJdbcTypeMapping("smalldatetime", JDBCType.TIMESTAMP);
        addJdbcTypeMapping("smallint", JDBCType.SMALLINT);
        addJdbcTypeMapping("smallmoney", JDBCType.DECIMAL);
        addJdbcTypeMapping("sql_variant", JDBCType.VARCHAR);
        addJdbcTypeMapping("sysname", JDBCType.VARCHAR);
        addJdbcTypeMapping("text", JDBCType.LONGVARCHAR);
        addJdbcTypeMapping("timestamp", JDBCType.BINARY);
        addJdbcTypeMapping("tinyint", JDBCType.TINYINT);
        addJdbcTypeMapping("uniqueidentifier", JDBCType.CHAR);
        addJdbcTypeMapping("varbinary", JDBCType.VARBINARY);
        addJdbcTypeMapping("varchar", JDBCType.VARCHAR);
        addJdbcTypeMapping("nvarchar", JDBCType.NVARCHAR);

    }

    @Override
    public String getQuoteStart() {
        return "[";
    }

    @Override
    public String getQuoteEnd() {
        return "]";
    }

    @Override
    public boolean isColumnToUpperCase() {
        return false;
    }


    @Override
    public String getId() {
        return "microsoft-mssql-server";
    }

    @Override
    public String getName() {
        return "Microsoft MSSQL Server";
    }
}
