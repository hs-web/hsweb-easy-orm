package org.hswebframework.ezorm.rdb.operator.dml.upsert;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;

import java.util.*;
import java.util.function.Consumer;

@SuppressWarnings("all")
public class DefaultUpsertOperator extends UpsertOperator {
    @Getter
    private UpsertOperatorParameter parameter = new UpsertOperatorParameter();

    @Setter
    private RDBTableMetadata table;

    private boolean columnValueModel = false;

    public static DefaultUpsertOperator of(RDBTableMetadata table) {
        DefaultUpsertOperator operator = new DefaultUpsertOperator();

        operator.setTable(table);

        return operator;
    }

    @Override
    public UpsertOperator ignoreUpdate(String... columns) {
        for (UpsertColumn column : parameter.getColumns()) {
            for (String col : columns) {
                if (column.getColumn().equals(col)) {
                    column.setUpdateIgnore(true);
                }
            }
        }
        return this;
    }

    @Override
    public UpsertOperator columns(String... columns) {
        for (String column : columns) {
            parameter.getColumns().add(UpsertColumn.of(column, false));
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

        Set<String> keys = new LinkedHashSet<>();

        for (Map<String, Object> value : values) {
            keys.addAll(value.keySet());
        }

        columns(keys.toArray(new String[0]));

        for (Map<String, Object> value : values) {
            values(keys.stream()
                       .map(value::get)
                       .toArray());
        }

        return this;
    }

    @Override
    public UpsertOperator where(Consumer<Conditional<?>> dsl) {
        Query<?, QueryParam> query = Query.of();
        dsl.accept(query);

        parameter.getWhere()
                 .addAll(query.getParam().getTerms());

        return this;
    }

    @Override
    public UpsertOperator doNothingOnConflict(boolean doNothing) {
        parameter.setDoNothingOnConflict(doNothing);
        return this;
    }

    @Override
    public UpsertOperator value(String column, Object value, boolean ignoreUpdate) {
        if (columnValueModel) {
            throw new UnsupportedOperationException("columns or values already set");
        }
        parameter.getColumns().add(UpsertColumn.of(column, ignoreUpdate));
        List<List<Object>> values = parameter.getValues();
        if (values.isEmpty()) {
            values.add(new ArrayList<>());
        }
        values.get(0).add(value);
        return this;
    }

    @Override
    public UpsertOperator value(String column, Object value) {
        return value(column, value, false);
    }


    @Override
    public SaveResultOperator execute() {
        return table.findFeatureNow(SaveOrUpdateOperator.ID).execute(getParameter());
    }
}
