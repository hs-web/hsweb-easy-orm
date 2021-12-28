package org.hswebframework.ezorm.rdb.operator.dml.delete;

import lombok.Getter;
import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateColumn;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateResultOperator;

import java.util.function.Consumer;

@Getter
public class BuildParameterDeleteOperator extends DeleteOperator {

    private final DeleteOperatorParameter parameter = new DeleteOperatorParameter();


    @Override
    public DeleteOperator where(Consumer<Conditional<?>> dsl) {
        Query<?, QueryParam> query = Query.of();
        dsl.accept(query);

        parameter.getWhere()
                .addAll(query.getParam().getTerms());

        return this;
    }

    @Override
    public BuildParameterDeleteOperator where(Term term) {
        parameter.getWhere().add(term);
        return this;
    }

    @Override
    public SqlRequest getSql() {
        throw new UnsupportedOperationException();
    }

    @Override
    public DeleteResultOperator execute() {
        throw new UnsupportedOperationException();
    }
}
