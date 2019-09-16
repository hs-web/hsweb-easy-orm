package org.hswebframework.ezorm.rdb.executor.wrapper;

public interface ColumnWrapperContext<T> {

    int getColumnIndex();

    String getColumnLabel();

    Object getResult();

    T getInstance();

    void setInstance(T instance);
}
