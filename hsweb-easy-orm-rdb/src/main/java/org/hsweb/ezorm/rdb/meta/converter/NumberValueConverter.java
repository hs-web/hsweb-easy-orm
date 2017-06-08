package org.hsweb.ezorm.rdb.meta.converter;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.core.ValueConverter;

import java.math.BigDecimal;

public class NumberValueConverter implements ValueConverter {

    private       Class   javaType;
    private final boolean isInt;
    private final boolean isDouble;
    private final boolean isLong;

    public NumberValueConverter(Class javaType) {
        this.javaType = javaType;
        isInt = javaType == int.class || javaType == Integer.class;
        isDouble = javaType == double.class || javaType == Double.class;
        isLong = javaType == long.class || javaType == Long.class;
    }

    @Override
    public Object getData(Object value) {
        if (StringUtils.isNullOrEmpty(value)) return null;
        if (value instanceof Number) return value;
        if (StringUtils.isNumber(value)) {
            BigDecimal decimal = new BigDecimal(String.valueOf(value));
            if (isInt) return decimal.intValue();
            if (isDouble) return decimal.doubleValue();
            if (isLong) return decimal.longValue();
            // TODO: 17-1-20  more type supports
            return decimal;
        }
        throw new UnsupportedOperationException("值" + value + "不为数字");
    }

    @Override
    public Object getValue(Object data) {
        if (data instanceof String) {
            if (StringUtils.isNumber(data)) {
                data = new BigDecimal(((String) data));
            }
        }
        if (data instanceof Number) {
            Number numberVal = ((Number) data);
            if (isInt) return numberVal.intValue();
            if (isDouble) return numberVal.doubleValue();
            if (isLong) return numberVal.longValue();
            // TODO: 17-1-20  more type supports
            return data;
        }
        return data;
    }
}
