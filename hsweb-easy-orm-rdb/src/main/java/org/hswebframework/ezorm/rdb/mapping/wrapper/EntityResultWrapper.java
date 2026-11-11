package org.hswebframework.ezorm.rdb.mapping.wrapper;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.utils.PropertyUtils;

import java.util.Optional;
import java.util.function.Supplier;

/**
 * 实体行包装器，按映射定位属性；直接 executor 传入原始值时自行解码，
 * 查询操作器已按同一列映射解码时直接赋值，不同目标映射仍执行其 codec。
 *
 * @see ColumnWrapperContext#getDecodedColumn()
 */
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
                .map(context::decode)
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
