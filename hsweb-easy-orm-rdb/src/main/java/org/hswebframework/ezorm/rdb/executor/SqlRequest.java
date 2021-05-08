package org.hswebframework.ezorm.rdb.executor;

import org.hswebframework.ezorm.rdb.utils.SqlUtils;

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

    default String toNativeSql(){
        return SqlUtils.toNativeSql(getSql(), getParameters());
    }
}
