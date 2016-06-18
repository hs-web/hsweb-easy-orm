package org.hsweb.ezorm.meta.converter;

import org.hsweb.commons.DateTimeUtils;
import org.hsweb.ezorm.meta.expand.ValueConverter;

import java.util.Date;

/**
 * Created by zhouhao on 16-6-5.
 */
public class DateTimeConverter implements ValueConverter {

    private String format;

    private Class toType;

    public DateTimeConverter(String format, Class toType) {
        this.format = format;
        this.toType = toType;
    }

    @Override
    public Object getData(Object value) {
        if (value instanceof Date) return value;
        if (value instanceof String) {
            return DateTimeUtils.formatUnknownString2Date(((String) value));
        }
        return value;
    }

    @Override
    public Object getValue(Object data) {
        if (data instanceof Number) {
            data = new Date(((Number) data).longValue());
        }
        if (data instanceof Date) {
            if (toType == Date.class) return data;
            if (toType == String.class) {
                return DateTimeUtils.format(((Date) data), format);
            }
        }
        if (data instanceof String) {
            if (toType == Date.class) {
                data = DateTimeUtils.formatUnknownString2Date(((String) data));
                if (data == null) data = DateTimeUtils.formatDateString(((String) data), format);
            }
        }
        return data;
    }
}
