package org.hswebframework.ezorm.rdb.operator.dml.upsert;

import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertResultOperator;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

public abstract class UpsertOperator {

    public abstract UpsertOperator columns(String... columns);

    public abstract UpsertOperator ignoreUpdate(String... columns);

    public abstract UpsertOperator values(Object... values);

    public abstract UpsertOperator value(String column, Object value);

    public abstract UpsertOperator value(String column, Object value,boolean ignoreUpdate);

    @SafeVarargs
    public final <T> UpsertOperator columns(StaticMethodReferenceColumn<T>... columns) {
        return columns(Arrays.stream(columns)
                .map(StaticMethodReferenceColumn::getColumn)
                .toArray(String[]::new));
    }

    @SafeVarargs
    public final <T> UpsertOperator values(MethodReferenceColumn<T>... columns) {
        String[] column = new String[columns.length];
        Object[] value = new Object[columns.length];

        for (int i = 0; i < columns.length; i++) {
            column[i] = columns[i].getColumn();
            value[i] = columns[i].get();
        }

        return columns(column).values(value);
    }

    public UpsertOperator value(Map<String, Object> values) {
        values.forEach(this::value);
        return this;
    }

    public abstract UpsertOperator values(List<Map<String, Object>> values);


    public abstract SaveResultOperator execute();

}
