package org.hswebframework.ezorm.rdb.supports.postgres;

import io.r2dbc.postgresql.codec.Vector;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.DataTypeBuilder;
import org.postgresql.util.PGobject;
import reactor.util.annotation.Nullable;

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.sql.JDBCType;
import java.sql.SQLType;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

@Getter
@RequiredArgsConstructor(staticName = "of")
public class VectorType implements DataType, ValueCodec<Object, Object>, DataTypeBuilder {

    public static final VectorType VECTOR = VectorType.of("vector");

    public static final VectorType HALF_VECTOR = VectorType.of("halfvec");

    public static final VectorType SPARSE_VECTOR = VectorType.of("sparsevec");

    private final String name;

    @Override
    public String getId() {
        return name;
    }

    @Override
    public SQLType getSqlType() {
        return JDBCType.OTHER;
    }

    @Override
    public Class<?> getJavaType() {
        return Float[].class;
    }

    @Override
    public boolean isLengthSupport() {
        return true;
    }

    @Override
    public Object encode(Object value) {
        return toFloat(value);
    }

    @Override
    public Object decode(Object data) {
        return toFloat(data);
    }

    private Float[] toFloat(Object data) {
        if (data == null) {
            return null;
        }
        if (PostgresqlDriverUtils.isR2dbcVector(data) && data instanceof Vector vector) {
            return toFloatArray(vector.getVector());
        }
        if (PostgresqlDriverUtils.isPgObject(data) && data instanceof PGobject vector) {
            return toFloatArray(vector.getValue());
        }
        return toFloatArray(data);
    }

    @Nullable
    public static Float[] toFloatArray(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof Float[] values) {
            return values;
        }
        if (value instanceof Collection<?> collection) {
            List<Float> values = new ArrayList<>(collection.size());
            for (Object element : collection) {
                values.add(parseFloat(element));
            }
            return values.toArray(new Float[0]);
        }
        if (value.getClass().isArray()) {
            int length = Array.getLength(value);
            Float[] result = new Float[length];
            for (int i = 0; i < length; i++) {
                result[i] = parseFloat(Array.get(value, i));
            }
            return result;
        }
        if (value instanceof CharSequence sequence) {
            return parseVector(sequence.toString());
        }
        if (value instanceof Number number) {
            return new Float[]{number.floatValue()};
        }
        return null;
    }

    private static Float[] parseVector(String vector) {
        if (vector == null) {
            return null;
        }
        String value = vector.trim();
        if (value.isEmpty()) {
            return new Float[0];
        }
        if ((value.startsWith("[") && value.endsWith("]"))
            || (value.startsWith("{") && value.endsWith("}"))
            || (value.startsWith("(") && value.endsWith(")"))) {
            value = value.substring(1, value.length() - 1).trim();
        }
        if (value.isEmpty()) {
            return new Float[0];
        }
        String[] parts = value.split("\\s*,\\s*");
        Float[] result = new Float[parts.length];
        for (int i = 0; i < parts.length; i++) {
            result[i] = parseFloat(parts[i]);
        }
        return result;
    }

    private static Float parseFloat(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof Float floatValue) {
            return floatValue;
        }
        if (value instanceof Number number) {
            return number.floatValue();
        }
        String str = String.valueOf(value).trim();
        if (StringUtils.isBlank(str)) {
            return null;
        }
        return new BigDecimal(str).floatValue();
    }

    @Override
    public String createColumnDataType(RDBColumnMetadata columnMetaData) {
        return switch (name) {
            case "vector" ->
                org.hswebframework.ezorm.core.utils.StringUtils.concat("vector(", columnMetaData.getLength(512), ")");
            case "halfvec" ->
                org.hswebframework.ezorm.core.utils.StringUtils.concat("halfvec(", columnMetaData.getLength(512), ")");
            case "sparsevec" ->
                org.hswebframework.ezorm.core.utils.StringUtils.concat("sparsevec(", columnMetaData.getLength(512), ")");
            default -> name;
        };
    }
}
