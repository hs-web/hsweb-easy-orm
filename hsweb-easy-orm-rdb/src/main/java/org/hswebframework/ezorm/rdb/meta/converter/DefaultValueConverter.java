package org.hswebframework.ezorm.rdb.meta.converter;


import org.hswebframework.ezorm.core.ValueConverter;

/**
 * Created by zhouhao on 16-6-4.
 */
public class DefaultValueConverter implements ValueConverter {
    @Override
    public Object getData(Object value) {
        return value;
    }

    @Override
    public Object getValue(Object data) {
        return data;
    }
}
