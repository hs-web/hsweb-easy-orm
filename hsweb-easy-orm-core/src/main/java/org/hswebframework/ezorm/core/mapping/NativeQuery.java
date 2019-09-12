package org.hswebframework.ezorm.core.mapping;

public interface NativeQuery<T> {

    NativeQuery<T> select(String... columns);

    NativeQuery<T> excludes(String... columns);

//    NativeQuery<T>

}
