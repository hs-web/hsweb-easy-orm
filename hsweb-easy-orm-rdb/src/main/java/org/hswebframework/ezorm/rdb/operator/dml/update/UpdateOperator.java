package org.hswebframework.ezorm.rdb.operator.dml.update;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;

import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Supplier;

public abstract class UpdateOperator {

    @SafeVarargs
    public final <T> UpdateOperator set(MethodReferenceColumn<T>... columnValues) {
        for (MethodReferenceColumn<T> columnValue : columnValues) {
            this.set(columnValue.getColumn(), columnValue.get());
        }
        return this;
    }

    @SafeVarargs
    public final UpdateOperator set(Supplier<? extends UpdateColumn>... columns) {
        for (Supplier<? extends UpdateColumn> column : columns) {
            set(column.get());
        }
        return this;
    }


    public UpdateOperator set(String column, String sql, Object... parameter) {
        return set(NativeSqlUpdateColumn.of(column, sql, parameter));
    }


    public UpdateOperator set(Map<String, Object> values) {
        values.forEach(this::set);
        return this;
    }

    public abstract UpdateOperator set(String column, Object value);
    public abstract UpdateOperator set(UpdateColumn column);
    public abstract UpdateOperator set(Object entity);

    public abstract UpdateOperator where(Consumer<Conditional<?>> dsl);

    public abstract UpdateOperator where(Term term);

    @SafeVarargs
    public final UpdateOperator where(Supplier<Term>... condition) {
        for (Supplier<Term> operator : condition) {
            where(operator.get());
        }
        return this;
    }

    public abstract SqlRequest getSql();

    public abstract UpdateResultOperator execute();

}
