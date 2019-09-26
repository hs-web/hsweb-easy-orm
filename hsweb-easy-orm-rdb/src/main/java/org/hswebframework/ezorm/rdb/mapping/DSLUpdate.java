package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.core.param.QueryParam;

import java.util.Arrays;

@SuppressWarnings("all")
public interface DSLUpdate<E, ME extends DSLUpdate> extends Conditional<ME> {

    ME includes(String... properties);

    ME excludes(String... properties);

    ME set(E entity);

    ME set(String column, Object value);

    ME setNull(String column);

    public QueryParam toQueryParam();

    default ME includes(StaticMethodReferenceColumn<E>... columns) {
        return includes(Arrays
                .stream(columns)
                .map(StaticMethodReferenceColumn::getColumn)
                .toArray(String[]::new));
    }

    default ME excludes(StaticMethodReferenceColumn<E>... columns) {
        return excludes(Arrays
                .stream(columns)
                .map(StaticMethodReferenceColumn::getColumn)
                .toArray(String[]::new));
    }

    default ME includes(MethodReferenceColumn<E>... columns) {
        return includes(Arrays
                .stream(columns)
                .map(MethodReferenceColumn::getColumn
                ).toArray(String[]::new));
    }

    default ME excludes(MethodReferenceColumn<E>... columns) {
        return excludes(Arrays
                .stream(columns)
                .map(MethodReferenceColumn::getColumn)
                .toArray(String[]::new));
    }

    default <R> ME set(MethodReferenceColumn<R> columnAndValue) {
        return set(columnAndValue.getColumn(), columnAndValue.get());
    }

    default ME set(StaticMethodReferenceColumn<E> column, Object value) {
        return set(column.getColumn(), value);
    }

}
