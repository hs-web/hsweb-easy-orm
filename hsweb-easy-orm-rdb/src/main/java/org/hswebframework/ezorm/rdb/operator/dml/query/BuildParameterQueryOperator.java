package org.hswebframework.ezorm.rdb.operator.dml.query;

import lombok.Getter;
import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.Operator;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Consumer;

public class BuildParameterQueryOperator extends QueryOperator {

    @Getter
    private QueryOperatorParameter parameter = new QueryOperatorParameter();

    public BuildParameterQueryOperator(String from){
        parameter.setFrom(from);
    }

    @Override
    public QueryOperator select(String... columns) {

        if (columns.length == 0) {

            return select("*");
        }
        for (String column : columns) {
            parameter.getSelect().add(SelectColumn.of(column));
        }
        return this;
    }

    @Override
    public QueryOperator select(MethodReferenceColumn... columns) {
        return select(Arrays.stream(columns)
                .map(MethodReferenceColumn::getColumn)
                .toArray(String[]::new));
    }

    @Override
    public QueryOperator select(SelectColumn... column) {
        for (SelectColumn selectColumn : column) {
            parameter.getSelect().add(selectColumn);
        }
        return this;
    }

    @Override
    public QueryOperator where(Consumer<Conditional<?>> conditionalConsumer) {
        Query<?, QueryParam> query = Query.of();
        conditionalConsumer.accept(query);
        parameter.getWhere().addAll(query.getParam().getTerms());
        return this;
    }

    @Override
    public QueryOperator where(Term term) {
        parameter.getWhere().add(term);
        return this;
    }

    @Override
    public QueryOperator where(Collection<Term> term) {
        parameter.getWhere().addAll(term);
        return this;
    }

    @Override
    public QueryOperator join(Join... joins) {
        for (Join join : joins) {
            parameter.getJoins().add(join);
        }
        return this;
    }

    @Override
    public QueryOperator groupBy(Operator... operators) {
        return this;
    }


    @Override
    public QueryOperator orderBy(SortOrder... operators) {
        for (SortOrder operator : operators) {
            parameter.getOrderBy().add(operator);
        }
        return this;
    }

    @Override
    public QueryOperator having(Operator... operators) {
        return this;
    }


    @Override
    public QueryOperator paging(int pageIndex, int pageSize) {
        parameter.setPageIndex(pageIndex);
        parameter.setPageSize(pageSize);
        return this;
    }

    @Override
    public QueryOperator forUpdate() {
        parameter.setForUpdate(true);
        return this;
    }

    @Override
    public SqlRequest getSql() {
        throw new UnsupportedOperationException();
    }

    @Override
    public <E, R> QueryResultOperator<E, R> fetch(ResultWrapper<E, R> wrapper) {
        throw new UnsupportedOperationException();
    }
}
