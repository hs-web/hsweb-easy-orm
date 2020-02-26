package org.hswebframework.ezorm.rdb.executor;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;

@Getter
@Setter
@AllArgsConstructor
public class DefaultColumnWrapperContext<T> implements ColumnWrapperContext<T> {

    private int columnIndex;

    private String columnLabel;

    private Object result;

    private T rowInstance;

}
