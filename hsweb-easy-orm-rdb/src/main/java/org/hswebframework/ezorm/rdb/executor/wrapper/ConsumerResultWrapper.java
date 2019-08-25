package org.hswebframework.ezorm.rdb.executor.wrapper;

import java.util.Objects;
import java.util.function.Consumer;

public class ConsumerResultWrapper<T> implements ResultWrapper<T, Integer> {

    private ResultWrapper<T, T> wrapper;

    private Consumer<T> consumer;

    private Runnable whenComplete;

    public ConsumerResultWrapper(ResultWrapper<T, T> wrapper, Consumer<T> consumer,Runnable whenComplete) {
        Objects.requireNonNull(wrapper);
        Objects.requireNonNull(consumer);

        this.wrapper = wrapper;
        this.consumer = consumer;
        this.whenComplete=whenComplete;
    }

    public ConsumerResultWrapper(ResultWrapper<T, T> wrapper, Consumer<T> consumer){
        this(wrapper,consumer,null);
    }

    private int counter;

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

        counter++;
        consumer.accept(result);
        return wrapper.completedWrapRow(rowIndex, result);
    }

    @Override
    public void completedWrap() {
        if(whenComplete!=null){
            whenComplete.run();
        }
    }

    @Override
    public Integer getResult() {
        return counter;
    }
}
