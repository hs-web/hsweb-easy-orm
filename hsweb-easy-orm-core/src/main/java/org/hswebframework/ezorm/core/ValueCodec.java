package org.hswebframework.ezorm.core;

public interface ValueCodec {
    Object encode(Object value);

    Object decode(Object data);
}
