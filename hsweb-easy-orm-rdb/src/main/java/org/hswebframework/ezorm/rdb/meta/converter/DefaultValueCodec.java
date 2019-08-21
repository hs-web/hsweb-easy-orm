package org.hswebframework.ezorm.rdb.meta.converter;


import org.hswebframework.ezorm.core.ValueCodec;

/**
 * Created by zhouhao on 16-6-4.
 */
public class DefaultValueCodec implements ValueCodec {
    @Override
    public Object encode(Object value) {
        return value;
    }

    @Override
    public Object decode(Object data) {
        return data;
    }
}
