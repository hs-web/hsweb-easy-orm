package org.hswebframework.ezorm.rdb.supports.clickhouse;

import org.hswebframework.ezorm.core.utils.StringUtils;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.dialect.DefaultDialect;

import java.sql.Date;
import java.sql.JDBCType;
/**
 * @className ClickhouseDire
 * @Description TODO
 * @Author dengpengyu
 * @Date 2023/9/4 14:53
 * @Vesion 1.0
 */
public class ClickhouseDialect extends DefaultDialect {

    public ClickhouseDialect() {
        super();
        addDataTypeBuilder(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.NVARCHAR, (meta) -> StringUtils.concat("nvarchar(", meta.getLength(), ")"));

        addDataTypeBuilder(JDBCType.TIMESTAMP, (meta) -> "datetime(" + Math.min(6, meta.getLength()) + ")");
        addDataTypeBuilder(JDBCType.TIME, (meta) -> "time");
        addDataTypeBuilder(JDBCType.DATE, (meta) -> "date");
        addDataTypeBuilder(JDBCType.CLOB, (meta) -> "text");
        addDataTypeBuilder(JDBCType.LONGVARBINARY, (meta) -> "blob");
        addDataTypeBuilder(JDBCType.LONGVARCHAR, (meta) -> "longtext");
        addDataTypeBuilder(JDBCType.BLOB, (meta) -> "blob");
        addDataTypeBuilder(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeBuilder(JDBCType.DOUBLE, (meta) -> "double");
        addDataTypeBuilder(JDBCType.INTEGER, (meta) -> "int");
        addDataTypeBuilder(JDBCType.NUMERIC, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(32), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.DECIMAL, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(32), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.TINYINT, (meta) -> "tinyint");
        addDataTypeBuilder(JDBCType.BOOLEAN, (meta) -> "tinyint");
        addDataTypeBuilder(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeBuilder(JDBCType.OTHER, (meta) -> "other");
        addDataTypeBuilder(JDBCType.LONGNVARCHAR, (meta) -> "text");

        addDataTypeBuilder("int", (meta) -> "int");
        addDataTypeBuilder("json", meta -> "json");

        registerDataType("clob", DataType.builder(JdbcDataType.of(JDBCType.CLOB, String.class), c -> "text"));
        registerDataType("longnvarchar", DataType.builder(JdbcDataType.of(JDBCType.LONGNVARCHAR, String.class), c -> "longtext"));
        registerDataType("longvarchar", DataType.builder(JdbcDataType.of(JDBCType.LONGVARCHAR, String.class), c -> "longtext"));

        registerDataType("int", JdbcDataType.of(JDBCType.INTEGER, Integer.class));
        registerDataType("text", JdbcDataType.of(JDBCType.CLOB, String.class));
        registerDataType("longtext", JdbcDataType.of(JDBCType.LONGVARCHAR, String.class));
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
        return "clickhouse";
    }

    @Override
    public String getName() {
        return "Clickhouse";
    }
}
