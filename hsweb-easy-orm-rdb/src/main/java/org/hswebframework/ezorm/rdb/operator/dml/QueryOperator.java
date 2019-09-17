package org.hswebframework.ezorm.rdb.operator.dml;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.LogicalOperation;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.*;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * database
 * .dml()
 * .query()
 * .column(count("id","total"))
 * .from("user")
 * .where(dsl->dsl.is("name","1"))
 * .execute()
 * .reactive(map())
 * .subscribe(data->);
 */
public abstract class QueryOperator implements LogicalOperation<QueryOperator> {

    public abstract QueryOperator select(String... columns);

    @SafeVarargs
    public final <T> QueryOperator select(StaticMethodReferenceColumn<T>... columns) {
        return select(Arrays.stream(columns)
                .map(StaticMethodReferenceColumn::getColumn)
                .toArray(String[]::new));
    }

    public abstract QueryOperator select(MethodReferenceColumn<?>... columns);

    public abstract QueryOperator select(SelectColumn... column);

    @SafeVarargs
    public final QueryOperator select(Supplier<SelectColumn>... operators) {
        for (Supplier<SelectColumn> operator : operators) {
            select(operator.get());
        }
        return this;
    }

    public abstract QueryOperator from(String name);

    public abstract QueryOperator where(Consumer<Conditional<?>> conditionalConsumer);

    public abstract QueryOperator where(Term term);

    public abstract QueryOperator where(Collection<Term> term);

    @SafeVarargs
    public final QueryOperator where(Supplier<Term>... condition) {
        for (Supplier<Term> operator : condition) {
            where(operator.get());
        }
        return this;
    }

    public abstract QueryOperator join(Join... on);

    @SafeVarargs
    public final QueryOperator join(Supplier<Join>... on) {
        for (Supplier<Join> joinOperator : on) {
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

    @SafeVarargs
    public final QueryOperator orderBy(Supplier<SortOrder>... operators) {
        for (Supplier<SortOrder> operator : operators) {
            orderBy(operator.get());
        }
        return this;
    }

    public abstract QueryOperator orderBy(SortOrder... operators);

    public abstract QueryOperator groupBy(Operator... operators);

    public abstract QueryOperator having(Operator... operators);

    public abstract QueryOperator paging(int pageIndex, int pageSize);

    public abstract QueryOperator forUpdate();

    public abstract SqlRequest getSql();

    public abstract <E, R> QueryResultOperator<E, R> fetch(ResultWrapper<E, R> wrapper);


}
