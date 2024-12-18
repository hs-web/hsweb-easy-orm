package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import java.io.Serializable;

public interface NativeSql extends Serializable {

    Object[] EMPTY_PARAMETER = new Object[0];

    String getSql();

    default Object[] getParameters() {
        return EMPTY_PARAMETER;
    }

    static NativeSql of(String sql, Object... parameters) {
        return new SimpleNativeSql(sql, parameters);
    }
}
