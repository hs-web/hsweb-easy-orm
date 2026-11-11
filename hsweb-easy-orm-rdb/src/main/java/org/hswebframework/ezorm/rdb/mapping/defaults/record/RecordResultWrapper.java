package org.hswebframework.ezorm.rdb.mapping.defaults.record;

import org.hswebframework.ezorm.rdb.executor.wrapper.AbstractMapResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;

/**
 * 将查询行转换为 Record。带映射的实例负责属性定位，原始列值按需解码；
 * 无映射时沿用 Map 的包装规则。结果收集由外层包装器负责。
 *
 * @see ColumnWrapperContext#isValueConverted()
 */
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
                .orElse(null);
            if (property == null) {
                return;
            }
            Object value = mapping
                .getColumnByProperty(property)
                .map(columnMetadata -> context.isValueConverted()
                    ? context.getResult()
                    : columnMetadata.decode(context.getResult()))
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
