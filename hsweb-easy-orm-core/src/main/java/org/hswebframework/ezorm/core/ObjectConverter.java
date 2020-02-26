package org.hswebframework.ezorm.core;

import java.util.function.Supplier;

public interface ObjectConverter {

    <T> T convert(Object from, Class<T> to);

    <T> T convert(Object from, Supplier<T> to);

}
