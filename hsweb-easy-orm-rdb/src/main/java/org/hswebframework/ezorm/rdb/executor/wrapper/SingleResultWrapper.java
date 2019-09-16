package org.hswebframework.ezorm.rdb.executor.wrapper;

public class SingleResultWrapper<T> implements ResultWrapper<T, T> {

    private ResultWrapper<T, T> wrapper;

    private T result;

    public SingleResultWrapper(ResultWrapper<T, T> wrapper) {
        this.wrapper = wrapper;
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
        wrapper.completedWrapRow(result);
        this.result = result;
        return false;
    }

    @Override
    public void completedWrap() {

    }

    @Override
    public T getResult() {
        return result;
    }
}
