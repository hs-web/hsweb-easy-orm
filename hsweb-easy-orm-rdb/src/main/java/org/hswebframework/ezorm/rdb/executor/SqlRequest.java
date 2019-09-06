package org.hswebframework.ezorm.rdb.executor;

public interface SqlRequest {

    String getSql();

    Object[] getParameters();

    boolean isEmpty();

    default boolean isNotEmpty() {
        return !isEmpty();
    }
}
