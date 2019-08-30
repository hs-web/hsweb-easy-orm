package org.hswebframework.ezorm.rdb.operator;

public interface NativeSql {
    String getSql();

    Object[] getParameters();
}
