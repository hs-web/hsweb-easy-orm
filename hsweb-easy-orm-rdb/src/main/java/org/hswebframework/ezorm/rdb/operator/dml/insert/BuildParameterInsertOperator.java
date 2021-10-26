package org.hswebframework.ezorm.rdb.operator.dml.insert;

import lombok.Getter;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;

import java.util.*;

public class BuildParameterInsertOperator extends InsertOperator {

    @Getter
    private final InsertOperatorParameter parameter = new InsertOperatorParameter();

    private boolean columnValueModel = false;

    @Override
    public InsertOperator columns(String... columns) {
        for (String column : columns) {
            parameter.getColumns().add(InsertColumn.of(column));
        }
        columnValueModel = true;
        return this;
    }

    @Override
    public InsertOperator values(Object... values) {
        parameter.getValues().add(Arrays.asList(values));
        columnValueModel = true;
        return this;
    }

    @Override
    public InsertOperator values(List<Map<String, Object>> values) {
        if (values == null || values.isEmpty()) {
            return this;
        }

        Set<String> keys = values.get(0).keySet();
        columns(keys.toArray(new String[0]));

        for (Map<String, Object> value : values) {
            values(keys
                    .stream()
                    .map(value::get)
                    .toArray());
        }

        return this;
    }

    @Override
    public InsertOperator value(String column, Object value) {
        if (columnValueModel) {
            throw new UnsupportedOperationException("columns or values already set");
        }
        parameter.getColumns().add(InsertColumn.of(column));
        List<List<Object>> values = parameter.getValues();
        if (values.isEmpty()) {
            values.add(new ArrayList<>());
        }
        values.get(0).add(value);

        return this;
    }

    @Override
    public SqlRequest getSql() {
        throw new UnsupportedOperationException();
    }

    @Override
    public InsertResultOperator execute() {
        throw new UnsupportedOperationException();
    }
}
