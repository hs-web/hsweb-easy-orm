package org.hswebframework.ezorm.rdb.operator.dml.delete;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;

import java.util.function.Consumer;
import java.util.function.Supplier;

public abstract class DeleteOperator {
    public abstract DeleteOperator where(Consumer<Conditional<?>> dsl);

    public abstract DeleteOperator where(Term term);

    /**
     * Copy a delete builder for subscription-local execution.
     * @return an independent builder with the same predicates
     * @since 4.2.3
     */
    public DeleteOperator copy() {
        throw new UnsupportedOperationException("this delete operator cannot be copied");
    }

    @SafeVarargs
    public final DeleteOperator where(Supplier<Term>... condition) {
        for (Supplier<Term> operator : condition) {
            where(operator.get());
        }
        return this;
    }

    public abstract SqlRequest getSql();

    public DeleteOperator accept(Consumer<DeleteOperator> consumer){
        consumer.accept(this);
        return this;
    }

    public abstract DeleteResultOperator execute();
}
