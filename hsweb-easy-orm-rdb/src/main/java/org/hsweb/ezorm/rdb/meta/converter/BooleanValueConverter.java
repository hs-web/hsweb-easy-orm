package org.hsweb.ezorm.rdb.meta.converter;

import org.hsweb.ezorm.core.ValueConverter;

public class BooleanValueConverter implements ValueConverter {

    @Override
    public Object getData(Object value) {
        return value;
    }

    @Override
    public Object getValue(Object data) {
        if (null == data) return false;
        if (data instanceof Boolean) return data;
        return "1".equals(String.valueOf(data)) || "true".equals(String.valueOf(data));
    }
}
