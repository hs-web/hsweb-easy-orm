package org.hswebframework.ezorm.rdb.orm.wrapper;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.rdb.config.GlobalConfig;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;

import java.util.Optional;
import java.util.function.Supplier;

public class EntityResultWrapper<E> implements ResultWrapper<E, E> {

    private Supplier<E> entityInstanceSupplier;

    @Getter
    @Setter
    private ObjectPropertyOperator propertyOperator = GlobalConfig.getPropertyOperator();

    @Getter
    @Setter
    private TableOrViewMetadata metadata;

    public EntityResultWrapper(Supplier<E> supplier) {
        this.entityInstanceSupplier = supplier;
    }

    @Override
    public E newRowInstance() {
        return entityInstanceSupplier.get();
    }

    @Override
    public void wrapColumn(ColumnWrapperContext<E> context) {
        String column = context.getColumnLabel();

        Object value = Optional.ofNullable(metadata)
                .flatMap(m -> m.findColumn(column))
                .map(columnMetadata -> columnMetadata.decode(context.getResult()))
                .orElseGet(context::getResult);
        propertyOperator.setProperty(context.getInstance(), column, value);
    }

    @Override
    public boolean completedWrapRow(E result) {
        return true;
    }

    @Override
    public E getResult() {
        return null;
    }
}
