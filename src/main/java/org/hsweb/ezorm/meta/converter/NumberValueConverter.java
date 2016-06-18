package org.hsweb.ezorm.meta.converter;

import org.hsweb.commons.ClassUtils;
import org.hsweb.ezorm.meta.expand.ValueConverter;

import java.math.BigDecimal;

/**
 * Created by zhouhao on 16-6-5.
 */
public class NumberValueConverter implements ValueConverter {

    private Class javaType;

    public NumberValueConverter(Class javaType) {
        this.javaType = javaType;
    }

    @Override
    public Object getData(Object value) {
        return value;
    }

    @Override
    public Object getValue(Object data) {
        if (data instanceof BigDecimal) {
            if (ClassUtils.instanceOf(javaType, Long.class)) {
                data = ((BigDecimal) data).longValue();
            } else if (ClassUtils.instanceOf(javaType, Double.class)) {
                data = ((BigDecimal) data).doubleValue();
            } else if (ClassUtils.instanceOf(javaType, Boolean.class)) {
                data = ((BigDecimal) data).longValue() != 0;
            } else if (ClassUtils.instanceOf(javaType, Float.class)) {
                data = ((BigDecimal) data).floatValue();
            } else if (ClassUtils.instanceOf(javaType, Integer.class)) {
                data = ((BigDecimal) data).intValue();
            } else {
                data = data.toString();
            }
        }
        return data;
    }
}
