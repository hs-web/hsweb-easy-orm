package org.hswebframework.ezorm.rdb.codec;

import lombok.Getter;
import org.hswebframework.ezorm.core.GlobalConfig;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.mapping.annotation.EnumCodec;
import org.hswebframework.ezorm.rdb.utils.PropertyUtils;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class EnumValueCodec implements ValueCodec<Object, Object> {

    @SuppressWarnings("rawtypes")
    private static final Collector collector = Collectors.joining(",");

    private static final Function<String, String[]> splitter = str -> str.split("[,]");

    static final String PROPERTY_NAME = EnumCodec.NAME,
        PROPERTY_ORDINAL = EnumCodec.ORDINAL;

    @SuppressWarnings("all")
    private final Class type;
    @SuppressWarnings("all")
    private final Object[] values;

    @Getter
    private boolean isArray;

    @Getter
    private boolean toMask;

    private final String property;

    public EnumValueCodec(Class<?> type) {
        this(type, PROPERTY_NAME);
    }

    public EnumValueCodec(Class<?> type, String property) {
        this.property = property;
        if (type.isArray()) {
            this.type = type.getComponentType();
            this.isArray = true;
        } else {
            this.type = type;
        }
        values = this.type.getEnumConstants();
        if (values == null) {
            throw new IllegalArgumentException(type + " must be enum");
        }
    }

    public EnumValueCodec(Class<?> type, String property, boolean toMask) {
        this(type, property);
        this.toMask = toMask;
    }

    public EnumValueCodec(Class<?> type, boolean toMask) {
        this(type);
        this.toMask = toMask;
    }

    private Object getValue(Enum<?> e) {
        switch (property) {
            case PROPERTY_NAME:
                return e.name();
            case PROPERTY_ORDINAL:
                return e.ordinal();
            default:
                return GlobalConfig
                    .getPropertyOperator()
                    .getProperty(e, property)
                    .orElseThrow(() -> new IllegalArgumentException("no property [" + property + "] found in enum " + e.getDeclaringClass()));
        }

    }

    @Override
    @SuppressWarnings("all")
    public Object encode(Object value) {

        if (value instanceof String && toMask) {
            String name = String.valueOf(value);
            value = Enum.valueOf(type, name);
        }

        if (value instanceof Enum) {
            if (!toMask) {
                return getValue(((Enum) value));
            } else {
                return enumToMask(((Enum) value));
            }
        }

        if (value instanceof Enum[]) {
            if (!toMask) {
                return Stream
                    .of(((Enum[]) value))
                    .map(this::getValue)
                    .collect(collector);
            } else {
                return enumToMask(((Enum[]) value));
            }
        }

        return value;
    }

    @Override
    @SuppressWarnings("rawtypes")
    public Object decode(Object data) {
        // 字符串
        if (data instanceof String) {
            if (!isArray) {
                for (Object value : values) {
                    Enum<?> e = ((Enum<?>) value);
                    if (eq(e, data)) {
                        return value;
                    }
                }
                return null;
            } else {
                List<String> arr = Arrays.asList(splitter.apply(((String) data)));
                return Stream
                    .of(values)
                    .map(Enum.class::cast)
                    .filter(e -> arr.contains(String.valueOf(getValue(e))))
                    .toArray(l -> (Enum[]) Array.newInstance(type, l));
            }
        }
        // 数字类型 toMask
        if (data instanceof Number && toMask) {
            long val = ((Number) data).longValue();
            Stream<Enum> stream = Stream
                .of(values)
                .map(Enum.class::cast)
                .filter(e -> enumInMask(val, e));

            if (isArray) {
                return stream.toArray(l -> (Enum<?>[]) Array.newInstance(type, l));
            } else {
                return stream.findFirst().orElse(null);
            }
        }

        Stream<Enum> stream = Stream
            .of(values)
            .map(Enum.class::cast)
            .filter(e -> eq(e, data));

        if (isArray) {
            return stream.toArray(l -> (Enum<?>[]) Array.newInstance(type, l));
        } else {
            return stream.findFirst().orElse(null);
        }

    }

    protected boolean eq(Enum<?> e, Object value) {
        Object val = getValue(e);
        // 忽略大小写对比字符串
        if (val instanceof String) {
            return ((String) val).equalsIgnoreCase(String.valueOf(value));
        }
        return GlobalConfig
            .getPropertyOperator()
            .compare(getValue(e), value) == 0;
    }

    private boolean enumInMask(long mask, Enum<?> e) {
        return (mask & (1L << e.ordinal())) != 0;
    }

    private long enumToMask(Enum<?>... enums) {
        if (enums == null) {
            return 0L;
        }
        long value = 0L;
        for (Enum<?> e : enums) {
            value |= (1L << e.ordinal());
        }
        return value;
    }
}
