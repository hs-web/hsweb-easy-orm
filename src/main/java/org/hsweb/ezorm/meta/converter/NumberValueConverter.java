package org.hsweb.ezorm.meta.converter;

import org.hsweb.commons.ClassUtils;
import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.meta.expand.ValueConverter;

import java.math.BigDecimal;

public class NumberValueConverter implements ValueConverter {

    private Class javaType;

    public NumberValueConverter(Class javaType) {
        this.javaType = javaType;
    }

    @Override
    public Object getData(Object value) {
        if (StringUtils.isNullOrEmpty(value)) return null;
        if (value instanceof Number) return value;
        throw new UnsupportedOperationException("值" + value + "不为数字");
    }

    @Override
    public Object getValue(Object data) {
        if (data instanceof Number) {
            if (ClassUtils.instanceOf(javaType, Long.class)) {
                data = ((Number) data).longValue();
            } else if (ClassUtils.instanceOf(javaType, Double.class)) {
                data = ((Number) data).doubleValue();
            } else if (ClassUtils.instanceOf(javaType, Boolean.class)) {
                data = ((Number) data).longValue() != 0;
            } else if (ClassUtils.instanceOf(javaType, Float.class)) {
                data = ((Number) data).floatValue();
            } else if (ClassUtils.instanceOf(javaType, Integer.class)) {
                data = ((Number) data).intValue();
            } else {
                data = data.toString();
            }
        }
        return data;
    }
}
