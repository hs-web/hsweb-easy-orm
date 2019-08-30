package org.hswebframework.ezorm.rdb.operator.dml;

import lombok.Getter;
import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.operator.DQLOperator;

import java.util.Arrays;
import java.util.function.Consumer;

public class DefaultQueryOperator extends QueryOperator {

    @Getter
    private ComplexQueryParameter parameter=new ComplexQueryParameter();

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
    public QueryOperator select(StaticMethodReferenceColumn... columns) {
        return select(Arrays.stream(columns)
                .map(StaticMethodReferenceColumn::getColumn)
                .toArray(String[]::new));
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
    public QueryOperator from(String name) {
        parameter.setFrom(name);
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
    public QueryOperator where(Operator... condition) {
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
    public QueryOperator having(Operator... operators) {
        return this;
    }

    @Override
    public QueryOperator limit(int limit, int offset) {
        parameter.setLimit(limit);
        parameter.setOffset(offset);
        return this;
    }

    @Override
    public QueryOperator forUpdate() {
        parameter.setForUpdate(true);
        return this;
    }

    @Override
    public SqlRequest getSql() {
        return null;
    }

    @Override
    public <E, R> DQLOperator<E,R> fetch(ResultWrapper<E, R> wrapper) {
        return null;
    }
}
