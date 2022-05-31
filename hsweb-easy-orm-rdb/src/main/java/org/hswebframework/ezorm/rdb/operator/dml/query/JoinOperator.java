package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.JoinType;

import java.util.function.Consumer;
import java.util.function.Supplier;

public class JoinOperator implements Supplier<Join> {

    private final Join join = new Join();

    public JoinOperator(String target, JoinType type) {
        join.setTarget(target);
        join.setType(type);
    }

    public final JoinOperator as(String alias) {
        join.setAlias(alias);

        return this;
    }

    public final JoinOperator on(String sql) {
        return on(Wheres.sql(sql));
    }

    public final JoinOperator on(Consumer<Conditional<?>> consumer) {
        Query<?, QueryParam> query = Query.of();
        consumer.accept(query);
        join.getTerms().addAll(query.getParam().getTerms());
        return this;
    }

    @SafeVarargs
    public final JoinOperator on(Supplier<Term>... conditions) {
        for (Supplier<Term> condition : conditions) {
            join.getTerms().add(condition.get());
        }
        return this;
    }

    @Override
    public Join get() {
        return join;
    }
}
