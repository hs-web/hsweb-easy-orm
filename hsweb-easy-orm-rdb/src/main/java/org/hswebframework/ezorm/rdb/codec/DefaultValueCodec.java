package org.hswebframework.ezorm.rdb.codec;


import org.hswebframework.ezorm.core.ValueCodec;

/**
 * @author zhouhao
 * @since 1.0
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
