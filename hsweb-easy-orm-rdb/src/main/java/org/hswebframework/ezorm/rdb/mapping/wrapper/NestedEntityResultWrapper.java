package org.hswebframework.ezorm.rdb.mapping.wrapper;

import org.hswebframework.ezorm.core.Extensible;
import org.hswebframework.ezorm.core.GlobalConfig;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.EntityPropertyDescriptor;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
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
                RDBColumnMetadata col = metadata.getTarget().getColumn(column).orElse(null);

                Object val = col == null ? context.getResult() : col.decode(context.getResult());

                setProperty(col, context.getRowInstance(), label, val);
            }
        } else {
            setProperty(mapping.getColumnByProperty(label).orElse(null),
                        context.getRowInstance(),
                        label,
                        context.getResult());
        }
    }

    protected void setProperty(RDBColumnMetadata col, E instance, String label, Object val) {
        if (instance instanceof Extensible && (col == null || !col
            .getFeature(EntityPropertyDescriptor.ID)
            .isPresent())) {
            ((Extensible) instance).setExtension(label, val);
        } else {
            GlobalConfig.getPropertyOperator().setProperty(instance, label, val);
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
