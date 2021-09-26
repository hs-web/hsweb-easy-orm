package org.hswebframework.ezorm.rdb.executor.wrapper;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.DefaultColumnWrapperContext;

@AllArgsConstructor(staticName = "of")
public class LowerCaseColumnResultWrapper<E, R> implements ResultWrapper<E, R> {

    private ResultWrapper<E, R> wrapper;

    @Override
    public E newRowInstance() {
        return wrapper.newRowInstance();
    }

    @Override
    public void beforeWrap(ResultWrapperContext context) {
        wrapper.beforeWrap(context);
    }

    @Override
    public void wrapColumn(ColumnWrapperContext<E> context) {

        wrapper.wrapColumn(
                new DefaultColumnWrapperContext<>(
                        context.getColumnIndex(),
                        context.getColumnLabel() == null
                                ? null
                                : context.getColumnLabel().toLowerCase(),
                        context.getResult(),
                        context.getRowInstance()
                )
        );

    }

    @Override
    public boolean completedWrapRow(E result) {
        return wrapper.completedWrapRow(result);
    }

    @Override
    public void completedWrap() {
        wrapper.completedWrap();
    }

    @Override
    public R getResult() {
        return wrapper.getResult();
    }
}
