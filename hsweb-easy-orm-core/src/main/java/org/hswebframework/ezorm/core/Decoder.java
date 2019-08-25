package org.hswebframework.ezorm.core;

public interface Decoder<T> {
    T decode(Object decode);
}
