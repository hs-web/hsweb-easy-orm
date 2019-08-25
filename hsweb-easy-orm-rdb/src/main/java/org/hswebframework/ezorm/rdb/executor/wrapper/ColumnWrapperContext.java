package org.hswebframework.ezorm.rdb.executor.wrapper;

public interface ColumnWrapperContext<T> {

    int getRowIndex();

    int getColumnIndex();

    String getColumnLabel();

    Object getResult();

    T getInstance();

}
