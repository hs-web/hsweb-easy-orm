package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.dialect.DefaultDialect;
import org.hswebframework.ezorm.core.utils.StringUtils;

import java.math.BigDecimal;
import java.sql.Date;
import java.sql.JDBCType;

/**
 * @author zhouhao
 */
public class SqlServerDialect extends DefaultDialect {

    public SqlServerDialect() {
        super();
        addDataTypeBuilder(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.NCHAR, (meta) -> StringUtils.concat("nchar(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.NVARCHAR, (meta) -> StringUtils.concat("nvarchar(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.TIMESTAMP, (meta) -> "datetime2");
        addDataTypeBuilder(JDBCType.TIME, (meta) -> "time");
        addDataTypeBuilder(JDBCType.DATE, (meta) -> "date");
        addDataTypeBuilder(JDBCType.CLOB, (meta) -> "text");
        addDataTypeBuilder(JDBCType.LONGVARBINARY, (meta) -> "varbinary(max)");
        addDataTypeBuilder(JDBCType.LONGVARCHAR, (meta) -> "text");
        addDataTypeBuilder(JDBCType.BLOB, (meta) -> "varbinary(max)");
        addDataTypeBuilder(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeBuilder(JDBCType.DOUBLE, (meta) -> "float");
        addDataTypeBuilder(JDBCType.FLOAT, (meta) -> "float");
        addDataTypeBuilder(JDBCType.INTEGER, (meta) -> "int");
        addDataTypeBuilder(JDBCType.NUMERIC, (meta) -> StringUtils.concat("numeric(", meta.getPrecision(32), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.DECIMAL, (meta) -> StringUtils.concat("numeric(", meta.getPrecision(32), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.TINYINT, (meta) -> "tinyint");
        addDataTypeBuilder(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeBuilder(JDBCType.OTHER, (meta) -> "other");
        addDataTypeBuilder(JDBCType.REAL, (meta) -> "real");

        registerDataType("longnvarchar", DataType.builder(JdbcDataType.of(JDBCType.LONGNVARCHAR, String.class), c -> "text"));
        registerDataType("longvarchar", DataType.builder(JdbcDataType.of(JDBCType.LONGVARCHAR, String.class), c -> "text"));

        registerDataType("datetime2", JdbcDataType.of(JDBCType.TIMESTAMP, java.util.Date.class));
        registerDataType("datetime", JdbcDataType.of(JDBCType.TIMESTAMP, java.util.Date.class));
        registerDataType("nvarchar", JdbcDataType.of(JDBCType.NVARCHAR, String.class));
        registerDataType("image", JdbcDataType.of(JDBCType.LONGVARBINARY, byte[].class));
        registerDataType("int", JdbcDataType.of(JDBCType.INTEGER, Integer.class));
        registerDataType("money", JdbcDataType.of(JDBCType.DECIMAL, Integer.class));
        registerDataType("nchar", JdbcDataType.of(JDBCType.CHAR, String.class));
        registerDataType("ntext", JdbcDataType.of(JDBCType.LONGVARCHAR, String.class));
        registerDataType("real", JdbcDataType.of(JDBCType.REAL, String.class));
        registerDataType("smalldatetime", JdbcDataType.of(JDBCType.TIMESTAMP, Date.class));
        registerDataType("smallint", JdbcDataType.of(JDBCType.SMALLINT, Short.class));
        registerDataType("smallmoney", JdbcDataType.of(JDBCType.DECIMAL, BigDecimal.class));
        registerDataType("text", JdbcDataType.of(JDBCType.CLOB, String.class));
        registerDataType("tinyint", JdbcDataType.of(JDBCType.TINYINT, Byte.class));
        registerDataType("varbinary", JdbcDataType.of(JDBCType.VARBINARY, byte[].class));
        registerDataType("tinyint", JdbcDataType.of(JDBCType.TINYINT, Byte.class));
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
