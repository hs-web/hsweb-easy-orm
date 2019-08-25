package org.hswebframework.ezorm.rdb.executor.jdbc;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;

@Getter
@Setter
@AllArgsConstructor
public class JdbcColumnWrapperContext<T> implements ColumnWrapperContext<T> {

    private int rowIndex;

    private int columnIndex;

    private String columnLabel;

    private Object result;

    private T instance;

}
