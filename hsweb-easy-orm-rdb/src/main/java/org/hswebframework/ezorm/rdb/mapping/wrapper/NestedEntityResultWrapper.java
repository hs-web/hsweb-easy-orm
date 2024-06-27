package org.hswebframework.ezorm.rdb.mapping.wrapper;

import org.hswebframework.ezorm.core.GlobalConfig;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyMetadata;

public class NestedEntityResultWrapper<E> implements ResultWrapper<E, E> {


    private final EntityColumnMapping mapping;

    public NestedEntityResultWrapper(EntityColumnMapping mapping) {
        this.mapping = mapping;
    }

    @Override
    public E newRowInstance() {
        return (E) mapping.newInstance();
    }

    @Override
    public void wrapColumn(ColumnWrapperContext<E> context) {
        String label = context.getColumnLabel();
        Object instance = context.getRowInstance();
        if (context.getResult() == null) {
            return;
        }
        if (label.contains(".")) {
            String[] nest = label.split("[.]", 2);
            String table = nest[0];
            String column = nest[1];
            Object nestInstance = GlobalConfig.getPropertyOperator().getPropertyOrNew(instance, table);
            if (null == nestInstance) {
                return;
            }
            ForeignKeyMetadata metadata = mapping.getTable().getForeignKey(table).orElse(null);
            if (null != metadata) {
                Object value = metadata
                    .getTarget()
                    .getColumn(column)
                    .map(m -> m.decode(context.getResult()))
                    .orElseGet(context::getResult);
                GlobalConfig.getPropertyOperator().setProperty(nestInstance, column, value);
            }
        } else {
            GlobalConfig.getPropertyOperator().setProperty(context.getRowInstance(), label, context.getResult());
        }
    }

    @Override
    public boolean completedWrapRow(E result) {
        return true;
    }

    @Override
    public E getResult() {
        throw new UnsupportedOperationException();
    }
}
