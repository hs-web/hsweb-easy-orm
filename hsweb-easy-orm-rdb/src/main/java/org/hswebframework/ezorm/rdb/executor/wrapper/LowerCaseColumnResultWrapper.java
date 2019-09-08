package org.hswebframework.ezorm.rdb.executor.wrapper;

import lombok.AllArgsConstructor;

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

        wrapper.wrapColumn(new ColumnWrapperContext<E>() {
            @Override
            public int getRowIndex() {
                return context.getRowIndex();
            }

            @Override
            public int getColumnIndex() {
                return context.getColumnIndex();
            }

            @Override
            public String getColumnLabel() {
                return context.getColumnLabel() == null ? null :
                        context.getColumnLabel().toLowerCase();
            }

            @Override
            public Object getResult() {
                return context.getResult();
            }

            @Override
            public E getInstance() {
                return context.getInstance();
            }
        });
    }

    @Override
    public boolean completedWrapRow(int rowIndex, E result) {
        return wrapper.completedWrapRow(rowIndex, result);
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
