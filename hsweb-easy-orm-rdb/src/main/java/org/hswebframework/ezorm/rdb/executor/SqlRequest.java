package org.hswebframework.ezorm.rdb.executor;

/**
 * @see SqlRequests
 */
public interface SqlRequest {

    String getSql();

    Object[] getParameters();

    boolean isEmpty();

    default boolean isNotEmpty() {
        return !isEmpty();
    }
}
