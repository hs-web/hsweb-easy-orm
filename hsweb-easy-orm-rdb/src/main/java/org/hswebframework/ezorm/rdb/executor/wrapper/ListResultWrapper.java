package org.hswebframework.ezorm.rdb.executor.wrapper;

import lombok.AllArgsConstructor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

@AllArgsConstructor
public class ListResultWrapper<T, C extends Collection<T>> implements ResultWrapper<T, C> {

    private ResultWrapper<T, ?> wrapper;

    private C collection;

    public static <T, C extends Collection<T>> ListResultWrapper<T, C> of(ResultWrapper<T, ?> wrapper, C container) {
        return new ListResultWrapper<>(wrapper, container);
    }

    @Override
    public T newRowInstance() {
        return wrapper.newRowInstance();
    }

    @Override
    public void beforeWrap(ResultWrapperContext context) {
        wrapper.beforeWrap(context);
    }

    @Override
    public void wrapColumn(ColumnWrapperContext<T> context) {
        wrapper.wrapColumn(context);
    }

    @Override
    public boolean completedWrapRow(T result) {
        collection.add(result);
        return wrapper.completedWrapRow(result);
    }

    @Override
    public void completedWrap() {
        wrapper.completedWrap();
    }

    @Override
    public C getResult() {

        return collection;
    }
}
