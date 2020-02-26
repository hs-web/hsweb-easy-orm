package org.hswebframework.ezorm.core;

public interface Encoder<T> {

    T encode(Object payload);
}
