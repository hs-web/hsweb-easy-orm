package org.hswebframework.ezorm.rdb.executor.wrapper;

public interface ColumnWrapperContext<T> {

    int getColumnIndex();

    String getColumnLabel();

    Object getResult();

    T getRowInstance();

    void setRowInstance(T instance);
}
