package org.hswebframework.ezorm.rdb.mapping.defaults.record;

import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;

import java.util.Optional;

public class RecordResultWrapper implements ResultWrapper<Record, Record> {

    public static RecordResultWrapper INSTANCE = new RecordResultWrapper();

    private EntityColumnMapping mapping;

    public static RecordResultWrapper of(EntityColumnMapping mapping) {
        RecordResultWrapper wrapper = new RecordResultWrapper();
        wrapper.mapping = mapping;
        return wrapper;
    }

    @Override
    public Record newRowInstance() {
        return new DefaultRecord();
    }

    @Override
    public void wrapColumn(ColumnWrapperContext<Record> context) {

        String property = Optional.ofNullable(mapping)
                .flatMap(mapping -> mapping.getPropertyByColumnName(context.getColumnLabel()))
                .orElse(context.getColumnLabel());

        Object value = Optional.ofNullable(mapping)
                .flatMap(mapping -> mapping.getColumnByName(context.getColumnLabel()))
                .map(columnMetadata -> columnMetadata.decode(context.getResult()))
                .orElseGet(context::getResult);

        context.getRowInstance().put(property, value);
    }

    @Override
    public boolean completedWrapRow(Record result) {
        return true;
    }

    @Override
    public Record getResult() {
       throw new UnsupportedOperationException();
    }
}
