package org.hswebframework.ezorm.rdb.operator.dml.update;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.operator.dml.TermSupplier;

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

    /**
     * Copy an update builder for subscription-local execution.
     * @return an independent builder with the same columns and predicates
     * @since 4.2.3
     */
    public UpdateOperator copy() {
        throw new UnsupportedOperationException("this update operator cannot be copied");
    }

    /**
     * Remove columns consumed by a non-column execution interceptor.
     * @param columns physical column names
     * @return this builder
     * @since 4.2.3
     */
    public UpdateOperator removeColumns(String... columns) {
        throw new UnsupportedOperationException("this update operator cannot remove columns");
    }

    @Deprecated
    public abstract UpdateOperator set(Object entity);

    public abstract UpdateOperator where(Consumer<Conditional<?>> dsl);

    public abstract UpdateOperator where(Term term);

    public final UpdateOperator where(TermSupplier... condition) {
        for (Supplier<Term> operator : condition) {
            where(operator.get());
        }
        return this;
    }

    public UpdateOperator accept(Consumer<UpdateOperator> consumer){
        consumer.accept(this);
        return this;
    }

    public abstract SqlRequest getSql();

    public abstract UpdateResultOperator execute();

}
