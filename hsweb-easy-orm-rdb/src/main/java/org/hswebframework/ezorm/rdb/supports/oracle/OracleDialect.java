package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.dialect.DefaultDialect;
import org.hswebframework.ezorm.core.utils.StringUtils;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SimpleSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import java.math.BigDecimal;
import java.sql.Date;
import java.sql.JDBCType;
import java.util.Arrays;
import java.util.Collections;

public class OracleDialect extends DefaultDialect {
    public OracleDialect() {
        super();
        addDataTypeBuilder(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.NCHAR, (meta) -> StringUtils.concat("nchar(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar2(", meta.getLength(), " char)"));
        addDataTypeBuilder(JDBCType.NVARCHAR, (meta) -> StringUtils.concat("nvarchar2(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.TIMESTAMP, (meta) -> "timestamp");
        addDataTypeBuilder(JDBCType.TIME, (meta) -> "timestamp");
        addDataTypeBuilder(JDBCType.DATE, (meta) -> "date");
        addDataTypeBuilder(JDBCType.TINYINT, (meta) -> "number(" + meta.getPrecision(2) + ")");
        addDataTypeBuilder(JDBCType.SMALLINT, (meta) ->"number(" + meta.getPrecision(5) + ")");
        addDataTypeBuilder(JDBCType.INTEGER, (meta) -> "number(" + meta.getPrecision(20) + ")");
        addDataTypeBuilder(JDBCType.DOUBLE, (meta) -> "binary_double");
        addDataTypeBuilder(JDBCType.CLOB, (meta) -> "clob");
        addDataTypeBuilder(JDBCType.LONGNVARCHAR, (meta) -> "clob");
        addDataTypeBuilder(JDBCType.BLOB, (meta) -> "blob");
        addDataTypeBuilder(JDBCType.BINARY, (meta) -> "blob");
        addDataTypeBuilder(JDBCType.NUMERIC, (meta) -> StringUtils.concat("number(", meta.getPrecision(38), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.DECIMAL, (meta) -> StringUtils.concat("number(", meta.getPrecision(38), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.BIGINT, (meta) -> "number(38,0)");
        addDataTypeBuilder(JDBCType.OTHER, (meta) -> "other");
        addDataTypeBuilder(JDBCType.BOOLEAN, (meta) -> "number(1)");
        addDataTypeBuilder(JDBCType.TINYINT, (meta) -> "number(1)");
        addDataTypeBuilder(JDBCType.LONGVARCHAR, (meta) -> "clob");
        addDataTypeBuilder(JDBCType.LONGVARBINARY, (meta) -> "blob");

        classJDBCTypeMapping.put(Boolean.class,JDBCType.TINYINT);

        registerDataType("number", DataType
                .builder(DataType.jdbc(JDBCType.NUMERIC, BigDecimal.class),
                         column -> StringUtils.concat("number(", column.getPrecision(38), ",", column.getScale(), ")")));

        registerDataType("longnvarchar", DataType.builder(JdbcDataType.of(JDBCType.LONGNVARCHAR, String.class), c -> "clob"));
        registerDataType("longvarchar", DataType.builder(JdbcDataType.of(JDBCType.LONGVARCHAR, String.class), c -> "clob"));

        registerDataType("varchar2", DataType.builder(DataType.jdbc(JDBCType.VARCHAR, String.class),
                                                      column -> "varchar2(" + column.getLength() + " char)"));

        registerDataType("nvarchar2", DataType.builder(DataType.jdbc(JDBCType.VARCHAR, String.class),
                                                       column -> "nvarchar2(" + column.getLength() + ")"));

        registerDataType("date", JdbcDataType.of(JDBCType.TIMESTAMP, Date.class));
        registerDataType("clob", JdbcDataType.of(JDBCType.LONGVARCHAR, String.class));
        registerDataType("blob", JdbcDataType.of(JDBCType.LONGVARBINARY, byte[].class));

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

    @Override
    public SqlFragments bitAnd(String column, long value) {
        return SimpleSqlFragments.of(
            Arrays.asList("BITAND(", column, ",", "?)"),
            Collections.singletonList(value)
        );
    }
}
