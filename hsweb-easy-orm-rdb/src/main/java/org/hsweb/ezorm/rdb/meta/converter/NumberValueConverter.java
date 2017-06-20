package org.hsweb.ezorm.rdb.meta.converter;

import org.hswebframework.utils.ClassUtils;
import org.hswebframework.utils.StringUtils;
import org.hsweb.ezorm.core.ValueConverter;
import org.hswebframework.utils.time.DateFormatter;

import java.math.BigDecimal;
import java.util.Date;

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
        if (value instanceof Date) {
            value = ((Date) value).getTime();
        } else if (!StringUtils.isNumber(value)) {
            Date date = DateFormatter.fromString(String.valueOf(value));
            if (null != date) value = date.getTime();
        }
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
            if (ClassUtils.instanceOf(javaType, Date.class)) {
                try {
                    Date date = (Date) javaType.newInstance();
                    date.setTime(numberVal.longValue());
                } catch (Exception ignore) {
                }
            }
            return data;
        }
        return data;
    }
}
