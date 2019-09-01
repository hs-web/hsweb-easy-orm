package org.hswebframework.ezorm.rdb.operator.builder.fragments;

public interface NativeSql {
    String getSql();

    Object[] getParameters();
}
