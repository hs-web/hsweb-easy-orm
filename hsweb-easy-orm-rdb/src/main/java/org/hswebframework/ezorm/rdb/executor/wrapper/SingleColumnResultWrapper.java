package org.hswebframework.ezorm.rdb.executor.wrapper;

import org.hswebframework.ezorm.core.Decoder;


public class SingleColumnResultWrapper<R> implements ResultWrapper<R, R> {

    private String column;

    private Decoder<R> decoder;

    public SingleColumnResultWrapper(String column, Decoder<R> decoder) {
        this.column = column;
        this.decoder = decoder;
    }

    @Override
    public R newRowInstance() {
        return null;
    }

    @Override
    public void wrapColumn(ColumnWrapperContext<R> context) {
        if (column.equalsIgnoreCase(context.getColumnLabel())) {
            context.setRowInstance(decoder.decode(context.getResult()));
        }
    }

    @Override
    public boolean completedWrapRow( R result) {

        return true;
    }

    @Override
    public R getResult() {
        throw new UnsupportedOperationException();
    }
}
