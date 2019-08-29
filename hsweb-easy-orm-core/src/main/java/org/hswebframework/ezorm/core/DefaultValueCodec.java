package org.hswebframework.ezorm.core;


/**
 * @author zhouhao
 * @since 1.0
 */
public class DefaultValueCodec implements ValueCodec {

    public static final DefaultValueCodec INSTANCE = new DefaultValueCodec();

    @Override
    public Object encode(Object value) {
        return value;
    }

    @Override
    public Object decode(Object data) {
        return data;
    }
}
