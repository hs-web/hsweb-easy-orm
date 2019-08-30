package org.hswebframework.ezorm.rdb.operator.builder.fragments;

public interface NativeSqlFragments {
    String getSql();

    Object[] getParameters();
}
