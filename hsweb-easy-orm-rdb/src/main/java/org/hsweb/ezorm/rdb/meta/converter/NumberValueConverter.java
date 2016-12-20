package org.hsweb.ezorm.rdb.meta.converter;

import org.hsweb.ezorm.core.ValueConverter;
import org.hswebframwork.utils.ClassUtils;
import org.hswebframwork.utils.StringUtils;

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
