package org.hswebframework.ezorm.rdb.mapping.defaults.record;

import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;

public class RecordResultWrapper implements ResultWrapper<Record, Record> {

    public static RecordResultWrapper INSTANCE=new RecordResultWrapper();

    @Override
    public Record newRowInstance() {
        return new DefaultRecord();
    }

    @Override
    public void wrapColumn(ColumnWrapperContext<Record> context) {
        context.getRowInstance().put(context.getColumnLabel(), context.getResult());
    }

    @Override
    public boolean completedWrapRow(Record result) {
        return true;
    }

    @Override
    public Record getResult() {
        return null;
    }
}
