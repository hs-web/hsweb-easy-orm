package org.hswebframework.ezorm.rdb.operator.dml;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.operator.DQLOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.JoinOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.Joins;

import java.util.function.Consumer;

/**
 * database
 * .dml()
 * .query()
 * .select(count("id","total"))
 * .from("user")
 * .where(dsl->dsl.is("name","1"))
 * .execute()
 * .reactive(map())
 * .subscribe(data->);
 */
public abstract class QueryOperator {

    public abstract QueryOperator select(String... columns);

    public abstract QueryOperator select(StaticMethodReferenceColumn... columns);

    public abstract QueryOperator select(MethodReferenceColumn... columns);

    public abstract QueryOperator select(SelectColumn... column);

    @SafeVarargs
    public final QueryOperator select(Operator<SelectColumn>... operators) {
        for (Operator<SelectColumn> operator : operators) {
            select(operator.get());
        }
        return this;
    }

    public abstract QueryOperator from(String name);

    public abstract QueryOperator where(Consumer<Conditional<?>> conditionalConsumer);

    public abstract QueryOperator where(Operator... condition);

    public abstract QueryOperator join(Join... on);

    @SafeVarargs
    public final QueryOperator join(Operator<Join>... on) {
        for (Operator<Join> joinOperator : on) {
            join(joinOperator.get());
        }
        return this;
    }

    public final QueryOperator leftJoin(String target, Consumer<JoinOperator> joinOperatorConsumer) {
        JoinOperator operator = Joins.left(target);
        joinOperatorConsumer.accept(operator);
        return join(operator.get());
    }

    public final QueryOperator innerJoin(String target, Consumer<JoinOperator> joinOperatorConsumer) {
        JoinOperator operator = Joins.inner(target);
        joinOperatorConsumer.accept(operator);
        return join(operator.get());
    }

    public final QueryOperator rightJoin(String target, Consumer<JoinOperator> joinOperatorConsumer) {
        JoinOperator operator = Joins.right(target);
        joinOperatorConsumer.accept(operator);
        return join(operator.get());
    }

    public final QueryOperator fullJoin(String target, Consumer<JoinOperator> joinOperatorConsumer) {
        JoinOperator operator = Joins.right(target);
        joinOperatorConsumer.accept(operator);
        return join(operator.get());
    }

    public abstract QueryOperator groupBy(Operator... operators);

    public abstract QueryOperator having(Operator... operators);

    public abstract QueryOperator limit(int limit, int offset);

    public abstract QueryOperator forUpdate();

    public abstract SqlRequest getSql();

    public abstract <E, R> DQLOperator<E, R> fetch(ResultWrapper<E, R> wrapper);


}
