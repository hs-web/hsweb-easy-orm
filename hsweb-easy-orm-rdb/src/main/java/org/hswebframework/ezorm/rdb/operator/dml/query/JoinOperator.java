package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.JoinType;
import org.hswebframework.ezorm.rdb.operator.dml.Operator;

import java.util.function.Consumer;

public class JoinOperator implements Operator<Join> {

    private Join join = new Join();

    public JoinOperator(String target, JoinType type) {
        join.setTarget(target);
        join.setType(type);
    }

    public final JoinOperator as(String alias) {
        join.setAlias(alias);

        return this;
    }

    public final Operator<Join> on(String sql) {
        return on(Wheres.sql(sql));
    }

    public final Operator<Join> on(Consumer<Conditional<?>> consumer) {
        Query<?, QueryParam> query = Query.of();
        consumer.accept(query);
        join.getTerms().addAll(query.getParam().getTerms());
        return this;
    }

    @SafeVarargs
    public final Operator<Join> on(Operator<Term>... conditions) {
        for (Operator<Term> condition : conditions) {
            join.getTerms().add(condition.get());
        }
        return this;
    }

    @Override
    public Join get() {
        return join;
    }
}
