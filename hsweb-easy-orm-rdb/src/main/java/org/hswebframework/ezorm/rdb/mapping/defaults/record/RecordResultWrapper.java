package org.hswebframework.ezorm.rdb.mapping.defaults.record;

import org.hswebframework.ezorm.rdb.executor.wrapper.AbstractMapResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;

public class RecordResultWrapper extends AbstractMapResultWrapper<Record> {

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

        if (mapping != null) {
            Record record = context.getRowInstance();

            String property = mapping
                    .getPropertyByColumnName(context.getColumnLabel())
                    .orElse(context.getColumnLabel());

            Object value = mapping
                    .getColumnByProperty(property)
                    .map(columnMetadata -> columnMetadata.decode(context.getResult()))
                    .orElseGet(context::getResult);

            super.doWrap(record, property, value);
            return;
        }

        super.wrapColumn(context);

    }

    @Override
    public Record getResult() {
        throw new UnsupportedOperationException();
    }
}
