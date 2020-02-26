package org.hswebframework.ezorm.rdb.operator.dml.upsert;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;

import java.util.*;

@SuppressWarnings("all")
public class DefaultUpsertOperator extends UpsertOperator {
    @Getter
    private InsertOperatorParameter parameter = new InsertOperatorParameter();

    @Setter
    private RDBTableMetadata table;

    private boolean columnValueModel = false;

    public static DefaultUpsertOperator of(RDBTableMetadata table){
        DefaultUpsertOperator operator=new DefaultUpsertOperator();

        operator.setTable(table);

        return operator;
    }

    @Override
    public UpsertOperator columns(String... columns) {
        for (String column : columns) {
            parameter.getColumns().add(InsertColumn.of(column));
        }
        columnValueModel = true;
        return this;
    }

    @Override
    public UpsertOperator values(Object... values) {
        parameter.getValues().add(Arrays.asList(values));
        columnValueModel = true;
        return this;
    }

    @Override
    public UpsertOperator values(List<Map<String, Object>> values) {
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
    public UpsertOperator value(String column, Object value) {
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
    public SaveResultOperator execute() {
        return table.findFeatureNow(SaveOrUpdateOperator.ID).execute(getParameter());
    }
}
