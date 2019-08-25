package org.hswebframework.ezorm.rdb.executor.wrapper;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class ConvertResultWrapper<T, R, C> implements ResultWrapper<T, C> {

    private ResultWrapper<T, R> wrapper;

    private Function<R, C> converter;

    private List<T> list = new ArrayList<>();

    public ConvertResultWrapper(ResultWrapper<T, R> wrapper, Function<R, C> converter) {
        this.wrapper = wrapper;
        this.converter = converter;
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
    }

    @Override
    public C getResult() {
        return converter.apply(wrapper.getResult());
    }
}
