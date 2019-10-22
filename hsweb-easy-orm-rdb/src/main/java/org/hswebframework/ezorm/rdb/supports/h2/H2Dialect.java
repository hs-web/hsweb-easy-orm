package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.dialect.DataTypeBuilder;
import org.hswebframework.ezorm.rdb.metadata.dialect.DefaultDialect;
import org.hswebframework.utils.StringUtils;

import java.sql.JDBCType;

public class H2Dialect extends DefaultDialect {
    public H2Dialect() {
        super();
        addDataTypeBuilder(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.TIMESTAMP, (meta) -> "timestamp");
        addDataTypeBuilder(JDBCType.SMALLINT, (meta) -> "smallint");
        addDataTypeBuilder(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeBuilder(JDBCType.TIME, (meta) -> "timestamp");
        addDataTypeBuilder(JDBCType.DATE, (meta) -> "date");
        addDataTypeBuilder(JDBCType.CLOB, (meta) -> "clob");
        addDataTypeBuilder(JDBCType.BLOB, (meta) -> "blob");
        addDataTypeBuilder(JDBCType.INTEGER, (meta) -> "int");
        addDataTypeBuilder(JDBCType.NUMERIC, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(32), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.TINYINT, (meta) -> "tinyint");
        addDataTypeBuilder(JDBCType.DECIMAL, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(32), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.OTHER, (meta) -> "other");

        addDataTypeBuilder(JDBCType.LONGNVARCHAR, (meta) -> "clob");
        addDataTypeBuilder(JDBCType.LONGVARBINARY, (meta) -> "blob");

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
        return "h2database";
    }

    @Override
    public String getName() {
        return "H2";
    }
}
