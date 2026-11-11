package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.metadata.dialect.DefaultDialect;
import org.hswebframework.ezorm.core.utils.StringUtils;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BatchSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SimpleSqlFragments;

import java.math.BigDecimal;
import java.sql.JDBCType;
import java.time.ZonedDateTime;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;
import java.util.Collections;
import java.util.StringJoiner;

/**
 * @author zhouhao
 * @since 3.0
 */
public class PostgresqlDialect extends DefaultDialect {
    @Override
    protected String buildExactStringType(String dataType) {
        return dataType + " collate \"C\"";
    }



    public PostgresqlDialect() {

        addDataTypeBuilder(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(255), ")"));
        addDataTypeBuilder(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(255), ")"));
        addDataTypeBuilder(JDBCType.TIMESTAMP, (meta) -> "timestamp");
        addDataTypeBuilder(JDBCType.TIMESTAMP_WITH_TIMEZONE, (meta) -> "timestamptz");
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

        addDataTypeBuilder("vector", meta -> StringUtils.concat("vector(", meta.getLength(512), ")"));
        addDataTypeBuilder("halfvec", meta -> StringUtils.concat("halfvec(", meta.getLength(512), ")"));
        addDataTypeBuilder("sparsevec", meta -> StringUtils.concat("sparsevec(", meta.getLength(512), ")"));


        registerDataType("vector", VectorType.VECTOR);
        registerDataType("halfvec", VectorType.HALF_VECTOR);
        registerDataType("sparsevec", VectorType.SPARSE_VECTOR);

        registerDataType("varchar[]", PostgresqlArrayType.VARCHAR_ARRAY);
        registerDataType("text[]", PostgresqlArrayType.TEXT_ARRAY);
        registerDataType("smallint[]", PostgresqlArrayType.SMALLINT_ARRAY);
        registerDataType("integer[]", PostgresqlArrayType.INTEGER_ARRAY);
        registerDataType("bigint[]", PostgresqlArrayType.BIGINT_ARRAY);
        registerDataType("int2[]", PostgresqlArrayType.SMALLINT_ARRAY);
        registerDataType("int4[]", PostgresqlArrayType.INTEGER_ARRAY);
        registerDataType("int8[]", PostgresqlArrayType.BIGINT_ARRAY);
        registerDataType("_varchar", PostgresqlArrayType.VARCHAR_ARRAY);
        registerDataType("_text", PostgresqlArrayType.TEXT_ARRAY);
        registerDataType("_int2", PostgresqlArrayType.SMALLINT_ARRAY);
        registerDataType("_int4", PostgresqlArrayType.INTEGER_ARRAY);
        registerDataType("_int8", PostgresqlArrayType.BIGINT_ARRAY);
        registerDataType("character varying[]", PostgresqlArrayType.VARCHAR_ARRAY);

        registerDataType("json", JsonType.INSTANCE);
        registerDataType("jsonb", JsonbType.INSTANCE);
        registerDataType("clob", DataType.builder(JdbcDataType.of(JDBCType.LONGVARCHAR, String.class), (c) -> "text"));
        registerDataType("blob", DataType.builder(JdbcDataType.of(JDBCType.BLOB, String.class), (c) -> "bytea"));
        registerDataType("longnvarchar", DataType.builder(JdbcDataType.of(JDBCType.LONGNVARCHAR, String.class), c -> "text"));
        registerDataType("longvarchar", DataType.builder(JdbcDataType.of(JDBCType.LONGVARCHAR, String.class), c -> "text"));
        registerDataType("timestamptz", DataType
            .builder(JdbcDataType
                         .of(JDBCType.TIMESTAMP_WITH_TIMEZONE, ZonedDateTime.class),
                     c -> "timestamptz"));

        registerDataType("int8", JdbcDataType.of(JDBCType.BIGINT, Long.class));
        registerDataType("int4", JdbcDataType.of(JDBCType.INTEGER, Integer.class));
        registerDataType("int2", JdbcDataType.of(JDBCType.SMALLINT, Short.class));
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
    public SqlFragments buildValues(List<RDBColumnMetadata> columns, List<Object[]> rows) {
        if (rows.isEmpty() || columns.isEmpty()) return super.buildValues(columns, rows);
        PostgresqlArrayType[] types = new PostgresqlArrayType[columns.size()];
        for (int col = 0; col < columns.size(); col++) {
            types[col] = arrayType(columns.get(col));
            if (types[col] == null) return super.buildValues(columns, rows);
        }
        // 不再次调用 codec 或求默认值；表达式、自定义 binder 和未知编码保留原 VALUES 语义。
        for (Object[] row : rows) {
            if (row.length != columns.size()) throw new IllegalArgumentException("Value count does not match columns");
            for (int col = 0; col < row.length; col++) {
                Object value = row[col];
                if (value != null && !(value instanceof NullValue) && !types[col].getComponentType().isInstance(value)) {
                    return super.buildValues(columns, rows);
                }
            }
        }
        StringJoiner placeholders = new StringJoiner(",", "select * from unnest(", ")");
        List<Object> parameters = new ArrayList<>(columns.size());
        for (int col = 0; col < columns.size(); col++) {
            PostgresqlArrayType type = types[col];
            Object[] values = (Object[]) Array.newInstance(type.getComponentType(), rows.size());
            for (int row = 0; row < rows.size(); row++) {
                Object value = rows.get(row)[col];
                values[row] = value instanceof NullValue ? null : value;
            }
            placeholders.add("?::" + type.getId());
            parameters.add(PostgresqlArrayParameter.of(type, values));
        }
        return SimpleSqlFragments.of(Collections.singletonList(placeholders.toString()), parameters);
    }

    private PostgresqlArrayType arrayType(RDBColumnMetadata column) {
        if (!(column.getSqlType() instanceof JDBCType jdbcType)) return null;
        return switch (jdbcType) {
            case CHAR, VARCHAR -> PostgresqlArrayType.VARCHAR_ARRAY;
            case LONGVARCHAR, CLOB -> PostgresqlArrayType.TEXT_ARRAY;
            case TINYINT, SMALLINT -> PostgresqlArrayType.SMALLINT_ARRAY;
            case INTEGER -> PostgresqlArrayType.INTEGER_ARRAY;
            case BIGINT -> PostgresqlArrayType.BIGINT_ARRAY;
            case NUMERIC, DECIMAL -> PostgresqlArrayType.NUMERIC_ARRAY;
            case BOOLEAN -> PostgresqlArrayType.BOOLEAN_ARRAY;
            case REAL, FLOAT -> PostgresqlArrayType.REAL_ARRAY;
            case DOUBLE -> PostgresqlArrayType.DOUBLE_ARRAY;
            default -> null;
        };
    }

    @Override
    public SqlFragments buildLike(SqlFragments left,
                                  SqlFragments right,
                                  boolean not,
                                  boolean ignoreCase) {
        BatchSqlFragments fragments = new BatchSqlFragments(3,
                                                              left.getParameters().size() + right.getParameters().size());
        fragments.add(left);
        if (not) {
            fragments.add(SqlFragments.NOT);
        }
        return fragments
            .addSql(ignoreCase ? "ilike" : "like")
            .add(right);
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
