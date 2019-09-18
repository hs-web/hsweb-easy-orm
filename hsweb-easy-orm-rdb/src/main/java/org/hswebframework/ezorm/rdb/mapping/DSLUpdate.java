package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;

public interface DSLUpdate<E, ME extends DSLUpdate> extends Conditional<ME> {

    ME includes(String... properties);

    ME excludes(String... properties);

    ME set(E entity);

    ME set(String column, Object value);

    default <R> ME set(MethodReferenceColumn<R> columnAndValue) {
        return set(columnAndValue.getColumn(), columnAndValue.get());
    }

    default ME set(StaticMethodReferenceColumn<E> column, Object value) {
        return set(column.getColumn(), value);
    }

}
