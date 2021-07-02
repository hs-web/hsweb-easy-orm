package org.hswebframework.ezorm.rdb.codec;

import lombok.Getter;
import org.hswebframework.ezorm.core.ValueCodec;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class EnumValueCodec implements ValueCodec<Object, Object> {

    private Class type;

    @Getter
    private boolean isArray;

    @Getter
    private boolean toMask;

    public EnumValueCodec(Class type) {
        if (type.isArray()) {
            this.type = type.getComponentType();
            this.isArray = true;
        } else {
            this.type = type;
        }

    }

    public EnumValueCodec(Class type, boolean toMask) {
        this(type);
        this.toMask = toMask;
    }

    private Collector collector = Collectors.joining(",");

    private Function<String, String[]> splitter = str -> str.split("[,]");

    @Override
    @SuppressWarnings("all")
    public Object encode(Object value) {

        if (value instanceof String && toMask) {
            String name = String.valueOf(value);
            value = Enum.valueOf(type, name);
        }

        if (value instanceof Enum) {
            if (!toMask) {
                return ((Enum) value).name();
            } else {
                return enumToMask(((Enum) value));
            }
        }

        if (value instanceof Enum[]) {
            if (!toMask) {
                return Stream.of(((Enum[]) value))
                             .map(Enum::name)
                             .collect(collector);
            } else {
                return enumToMask(((Enum[]) value));
            }
        }

        return value;
    }

    @Override
    public Object decode(Object data) {
        if (data instanceof String) {
            if (!isArray) {
                return Stream.of(type.getEnumConstants())
                             .map(Enum.class::cast)
                             .filter(e -> e.name().equalsIgnoreCase(String.valueOf(data)))
                             .findFirst()
                             .orElse(null);
            } else {
                List<String> arr = Arrays.asList(splitter.apply(((String) data)));
                return Stream.of(type.getEnumConstants())
                             .map(Enum.class::cast)
                             .filter(e -> arr.contains(e.name()))
                             .toArray(l -> (Enum[]) Array.newInstance(type, l));
            }
        }
        if (data instanceof Number) {
            long val = ((Number) data).longValue();

            Stream<Enum> stream = Stream.of(type.getEnumConstants())
                                        .map(Enum.class::cast)
                                        .filter(e -> toMask ? enumInMask(val, e) : e.ordinal() == val);

            if (isArray) {
                return stream.toArray(l -> (Enum[]) Array.newInstance(type, l));
            } else {
                return stream.findFirst().orElse(null);
            }

        }

        return data;
    }

    private boolean enumInMask(long mask, Enum e) {
        return (mask & (1L << e.ordinal())) != 0;
    }

    private long enumToMask(Enum... enums) {
        if (enums == null) {
            return 0L;
        }
        long value = 0L;
        for (Enum e : enums) {
            value |= (1L << e.ordinal());
        }
        return value;
    }
}
