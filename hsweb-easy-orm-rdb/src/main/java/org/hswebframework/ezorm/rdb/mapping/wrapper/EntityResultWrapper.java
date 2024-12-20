package org.hswebframework.ezorm.rdb.mapping.wrapper;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.core.GlobalConfig;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.utils.PropertyUtils;

import java.util.Optional;
import java.util.function.Supplier;

public class EntityResultWrapper<E> implements ResultWrapper<E, E> {

    private final Supplier<E> entityInstanceSupplier;

    @Getter
    @Setter
    private EntityColumnMapping mapping;

    public EntityResultWrapper(Supplier<E> supplier) {
        this.entityInstanceSupplier = supplier;
    }

    public EntityResultWrapper(Supplier<E> supplier,EntityColumnMapping mapping) {
        this.entityInstanceSupplier=supplier;
        this.mapping=mapping;
    }

    @Override
    public E newRowInstance() {
        return entityInstanceSupplier.get();
    }

    @Override
    public void wrapColumn(ColumnWrapperContext<E> context) {
        String property = Optional.ofNullable(mapping)
                .flatMap(mapping -> mapping.getPropertyByColumnName(context.getColumnLabel()))
                .orElse(context.getColumnLabel());

        Object value = Optional.ofNullable(mapping)
                .flatMap(mapping -> mapping.getColumnByProperty(property))
                .map(columnMetadata -> columnMetadata.decode(context.getResult()))
                .orElseGet(context::getResult);
        if (value != null) {
           PropertyUtils.setProperty(context.getRowInstance(), property, value,mapping);
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
