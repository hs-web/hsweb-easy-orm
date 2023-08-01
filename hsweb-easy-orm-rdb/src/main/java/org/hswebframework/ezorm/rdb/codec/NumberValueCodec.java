package org.hswebframework.ezorm.rdb.codec;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.core.utils.StringUtils;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.utils.time.DateFormatter;

import java.lang.reflect.Constructor;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.function.Function;

public class NumberValueCodec implements ValueCodec<Object, Object> {

    private final Function<Number, Object> converter;

    public NumberValueCodec(Function<Number, Object> converter) {
        this.converter = converter;
    }

    @SneakyThrows
    public NumberValueCodec(Class<?> javaType) {
        if (javaType == int.class || javaType == Integer.class) {
            converter = Number::intValue;
        } else if (javaType == double.class || javaType == Double.class) {
            converter = Number::doubleValue;
        } else if (javaType == float.class || javaType == Float.class) {
            converter = Number::floatValue;
        } else if (javaType == long.class || javaType == Long.class) {
            converter = Number::longValue;
        } else if (javaType == byte.class || javaType == Byte.class) {
            converter = Number::byteValue;
        } else if (javaType == short.class || javaType == Short.class) {
            converter = Number::shortValue;
        } else if (javaType == boolean.class || javaType == Boolean.class) {
            converter = num -> num.byteValue() != 0;
        } else if (Date.class.isAssignableFrom(javaType)) {
            Constructor<?> constructor = javaType.getConstructor();
            converter = num -> {
                try {
                    Date date = (Date) constructor.newInstance();
                    date.setTime(num.longValue());
                    return date;
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            };
        } else {
            converter = num -> num;
        }
    }

    private Object tryCastNumber(Object value) {
        if (null == value) {
            return null;
        }

        if (value instanceof NullValue) {
            return value;
        }

        if (value instanceof Date) {
            value = ((Date) value).getTime();
        } else if (value instanceof LocalDateTime) {
            value = ((LocalDateTime) value).atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
        } else if (value instanceof String) {
            //尝试转换字符格式的日期
            Date date = DateFormatter.fromString(String.valueOf(value));
            if (null != date) {
                value = date.getTime();
            } else {
                value = new BigDecimal(String.valueOf(value));
            }
        }

        if (value instanceof Number) {
            return converter.apply(((Number) value));
        }

        if (Boolean.TRUE.equals(value)) {
            return converter.apply(1);
        }
        if (Boolean.FALSE.equals(value)) {
            return converter.apply(0);
        }
        throw new IllegalArgumentException("值" + value + "无法转换为数字");
    }

    @Override
    public Object encode(Object value) {
        if (StringUtils.isNullOrEmpty(value)) {
            return null;
        }
        return tryCastNumber(value);
    }


    @Override
    public Object decode(Object data) {
        return tryCastNumber(data);
    }
}
