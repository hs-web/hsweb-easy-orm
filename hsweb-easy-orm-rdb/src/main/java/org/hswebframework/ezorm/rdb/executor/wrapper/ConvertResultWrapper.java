package org.hswebframework.ezorm.rdb.executor.wrapper;

import java.util.function.Function;

public class ConvertResultWrapper<T, R, C> implements ResultWrapper<T, C> {

    private ResultWrapper<T, R> wrapper;

    private Function<R, C> converter;

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
    public boolean completedWrapRow(T result) {

        return wrapper.completedWrapRow(result);
    }

    @Override
    public void completedWrap() {
        wrapper.completedWrap();
    }

    @Override
    public C getResult() {
        return converter.apply(wrapper.getResult());
    }
}
