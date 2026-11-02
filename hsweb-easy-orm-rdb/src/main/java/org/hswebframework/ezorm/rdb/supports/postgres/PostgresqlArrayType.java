package org.hswebframework.ezorm.rdb.supports.postgres;

import io.r2dbc.postgresql.codec.PostgresqlObjectId;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.core.meta.ColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.DataTypeBuilder;
import org.postgresql.util.PGobject;

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.sql.JDBCType;
import java.sql.SQLException;
import java.sql.SQLType;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

@Getter
@RequiredArgsConstructor(staticName = "of")
public class PostgresqlArrayType implements DataType, ValueCodec<Object, Object>, DataTypeBuilder {

    public static final PostgresqlArrayType VARCHAR_ARRAY = PostgresqlArrayType.of("varchar[]", "varchar", PostgresqlObjectId.VARCHAR_ARRAY, String.class, String[].class);

    public static final PostgresqlArrayType TEXT_ARRAY = PostgresqlArrayType.of("text[]", "text", PostgresqlObjectId.TEXT_ARRAY, String.class, String[].class);

    public static final PostgresqlArrayType SMALLINT_ARRAY = PostgresqlArrayType.of("smallint[]", "int2", PostgresqlObjectId.INT2_ARRAY, Short.class, Short[].class);

    public static final PostgresqlArrayType INTEGER_ARRAY = PostgresqlArrayType.of("integer[]", "int4", PostgresqlObjectId.INT4_ARRAY, Integer.class, Integer[].class);

    public static final PostgresqlArrayType BIGINT_ARRAY = PostgresqlArrayType.of("bigint[]", "int8", PostgresqlObjectId.INT8_ARRAY, Long.class, Long[].class);

    private final String name;

    private final String jdbcElementType;

    private final PostgresqlObjectId r2dbcArrayType;

    private final Class<?> componentType;

    private final Class<?> javaType;

    @Override
    public String getId() {
        return name;
    }

    @Override
    public SQLType getSqlType() {
        return JDBCType.ARRAY;
    }

    @Override
    public Object encode(Object value) {
        return convert(value);
    }

    @Override
    public Object encode(Object value, ColumnMetadata column) {
        return PostgresqlArrayParameter.of(this, convert(value));
    }

    @Override
    public Object decode(Object data) {
        return convert(data);
    }

    @Override
    public String createColumnDataType(RDBColumnMetadata columnMetaData) {
        return name;
    }

    private Object convert(Object value) {
        if (value == null) {
            return null;
        }
        if (javaType.isInstance(value)) {
            return value;
        }
        if (value instanceof java.sql.Array sqlArray) {
            return convertSqlArray(sqlArray);
        }
        if (PostgresqlDriverUtils.isPgObject(value) && value instanceof PGobject pgObject) {
            return convert(pgObject.getValue());
        }
        if (value instanceof Collection<?> collection) {
            return convertCollection(collection);
        }
        if (value.getClass().isArray()) {
            return convertJavaArray(value);
        }
        if (value instanceof CharSequence sequence) {
            return parseArrayLiteral(sequence.toString());
        }
        return newArray(convertElement(value));
    }

    private Object convertSqlArray(java.sql.Array sqlArray) {
        try {
            return convert(sqlArray.getArray());
        } catch (SQLException e) {
            throw new IllegalArgumentException("Failed to read PostgreSQL array value", e);
        } finally {
            try {
                sqlArray.free();
            } catch (Exception ignore) {
            }
        }
    }

    private Object convertCollection(Collection<?> collection) {
        Object result = Array.newInstance(componentType, collection.size());
        int index = 0;
        for (Object element : collection) {
            Array.set(result, index++, convertElement(element));
        }
        return result;
    }

    private Object convertJavaArray(Object array) {
        int length = Array.getLength(array);
        Object result = Array.newInstance(componentType, length);
        for (int i = 0; i < length; i++) {
            Array.set(result, i, convertElement(Array.get(array, i)));
        }
        return result;
    }

    private Object parseArrayLiteral(String literal) {
        String value = StringUtils.trimToEmpty(literal);
        if (value.isEmpty()) {
            return Array.newInstance(componentType, 0);
        }
        if ((value.startsWith("{") && value.endsWith("}"))
            || (value.startsWith("[") && value.endsWith("]"))
            || (value.startsWith("(") && value.endsWith(")"))) {
            value = value.substring(1, value.length() - 1);
        }
        if (value.isEmpty()) {
            return Array.newInstance(componentType, 0);
        }
        List<Token> tokens = split(value);
        Object result = Array.newInstance(componentType, tokens.size());
        for (int i = 0; i < tokens.size(); i++) {
            Token token = tokens.get(i);
            Array.set(result, i, convertElement(token.value, token.quoted));
        }
        return result;
    }

    private List<Token> split(String value) {
        List<Token> tokens = new ArrayList<>();
        StringBuilder builder = new StringBuilder();
        boolean inQuotes = false;
        boolean escaping = false;
        boolean quoted = false;
        int depth = 0;
        for (int i = 0; i < value.length(); i++) {
            char ch = value.charAt(i);
            if (escaping) {
                builder.append(ch);
                escaping = false;
                continue;
            }
            if (inQuotes) {
                if (ch == '\\') {
                    escaping = true;
                    continue;
                }
                if (ch == '"') {
                    inQuotes = false;
                    quoted = true;
                    continue;
                }
                builder.append(ch);
                continue;
            }
            if (ch == '"') {
                inQuotes = true;
                continue;
            }
            if (ch == '{') {
                depth++;
                builder.append(ch);
                continue;
            }
            if (ch == '}') {
                if (depth > 0) {
                    depth--;
                }
                builder.append(ch);
                continue;
            }
            if (ch == ',' && depth == 0) {
                tokens.add(new Token(builder.toString(), quoted));
                builder.setLength(0);
                quoted = false;
                continue;
            }
            builder.append(ch);
        }
        tokens.add(new Token(builder.toString(), quoted));
        return tokens;
    }

    private Object convertElement(Object element) {
        return convertElement(element, false);
    }

    private Object convertElement(Object element, boolean quoted) {
        if (element == null) {
            return null;
        }
        if (componentType == String.class) {
            String value = String.valueOf(element);
            if (!quoted) {
                value = value.trim();
                if ("null".equalsIgnoreCase(value)) {
                    return null;
                }
            }
            return value;
        }
        if (componentType.isInstance(element)) {
            return element;
        }
        if (element instanceof Number number) {
            return fromNumber(number);
        }
        String value = quoted ? String.valueOf(element) : StringUtils.trimToNull(String.valueOf(element));
        if (value == null) {
            return null;
        }
        if (!quoted && "null".equalsIgnoreCase(value)) {
            return null;
        }
        if (componentType == Short.class) {
            return new BigDecimal(value).shortValue();
        }
        if (componentType == Integer.class) {
            return new BigDecimal(value).intValue();
        }
        if (componentType == Long.class) {
            return new BigDecimal(value).longValue();
        }
        return value;
    }

    private Object fromNumber(Number number) {
        if (componentType == Short.class) {
            return number.shortValue();
        }
        if (componentType == Integer.class) {
            return number.intValue();
        }
        if (componentType == Long.class) {
            return number.longValue();
        }
        return String.valueOf(number);
    }

    private Object newArray(Object value) {
        Object result = Array.newInstance(componentType, 1);
        Array.set(result, 0, value);
        return result;
    }

    @RequiredArgsConstructor
    private static class Token {
        private final String value;
        private final boolean quoted;
    }
}
