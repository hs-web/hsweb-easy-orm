package org.hswebframework.ezorm.rdb.events;

public interface ContextKey<T> {
    String getKey();

    static <T> ContextKey<T> of(String key) {
        return () -> key;
    }
}
