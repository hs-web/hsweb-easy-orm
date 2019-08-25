package org.hswebframework.ezorm.rdb.executor.wrapper;

import lombok.AllArgsConstructor;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

@AllArgsConstructor
public class ListResultWrapper<T> implements ResultWrapper<T, List<T>> {

    private ResultWrapper<T, ?> wrapper;

    private List<T> list;


    public static <T> ListResultWrapper<T> arrayList(ResultWrapper<T, ?> wrapper) {
        return new ListResultWrapper<>(wrapper, new ArrayList<>());
    }

    public static <T> ListResultWrapper<T> linkedList(ResultWrapper<T, ?> wrapper) {
        return new ListResultWrapper<>(wrapper, new LinkedList<>());
    }

    public static <T> ListResultWrapper<T> of(ResultWrapper<T, ?> wrapper, List<T> container) {
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
    public boolean completedWrapRow(int rowIndex, T result) {
        list.add(result);
        return wrapper.completedWrapRow(rowIndex, result);
    }

    @Override
    public void completedWrap() {
        wrapper.completedWrap();
    }

    @Override
    public List<T> getResult() {

        return list;
    }
}
