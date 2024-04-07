package org.hswebframework.ezorm.rdb.operator.dml.update;

import lombok.Getter;
import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;

import java.util.function.Consumer;

@Getter
public class BuildParameterUpdateOperator extends UpdateOperator {

    private final UpdateOperatorParameter parameter = new UpdateOperatorParameter();

    @Override
    public UpdateOperator set(String column, Object value) {
        UpdateColumn updateColumn = new UpdateColumn();
        updateColumn.setColumn(column);
        updateColumn.setValue(value);
        return set(updateColumn);
    }

    @Override
    public UpdateOperator set(Object entity) {
       throw new UnsupportedOperationException();
    }

    @Override
    public UpdateOperator set(UpdateColumn column) {
        parameter.getColumns().remove(column);
        parameter.getColumns().add(column);
        return this;
    }

    @Override
    public UpdateOperator where(Consumer<Conditional<?>> dsl) {
        Query<?, QueryParam> query = Query.of();
        dsl.accept(query);

        parameter.getWhere()
                .addAll(query.getParam().getTerms());

        return this;
    }

    @Override
    public UpdateOperator where(Term term) {
        parameter.getWhere().add(term);
        return this;
    }

    @Override
    public SqlRequest getSql() {
        throw new UnsupportedOperationException();
    }

    @Override
    public UpdateResultOperator execute() {
        throw new UnsupportedOperationException();
    }
}
