package org.hswebframework.ezorm.rdb.operator.dml.delete;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;

import java.util.function.Consumer;
import java.util.function.Supplier;

public abstract class DeleteOperator {
    public abstract DeleteOperator where(Consumer<Conditional<?>> dsl);

    public abstract DeleteOperator where(Term term);

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
