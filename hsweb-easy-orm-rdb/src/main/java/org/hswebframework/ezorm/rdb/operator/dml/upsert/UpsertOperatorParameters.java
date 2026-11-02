package org.hswebframework.ezorm.rdb.operator.dml.upsert;

import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

import java.util.ArrayList;
import java.util.List;

public final class UpsertOperatorParameters {

    private UpsertOperatorParameters() {
    }

    public static UpsertOperatorParameter ensureRuntimeDefaultPrimaryKey(UpsertOperatorParameter parameter,
                                                                         RDBTableMetadata table) {
        RDBColumnMetadata primaryKey = table
            .getColumns()
            .stream()
            .filter(RDBColumnMetadata::isPrimaryKey)
            .findFirst()
            .orElse(null);

        return ensureRuntimeDefaultPrimaryKey(parameter, primaryKey);
    }

    public static UpsertOperatorParameter ensureRuntimeDefaultPrimaryKey(UpsertOperatorParameter parameter,
                                                                         RDBColumnMetadata primaryKey) {
        if (primaryKey == null
            || !(primaryKey.getDefaultValue() instanceof RuntimeDefaultValue)
            || hasColumn(parameter, primaryKey)) {
            return parameter;
        }

        UpsertOperatorParameter copy = new UpsertOperatorParameter();
        copy.setDoNothingOnConflict(parameter.isDoNothingOnConflict());
        copy.getWhere().addAll(parameter.getWhere());
        copy.getColumns().addAll(parameter.getColumns());
        copy.getColumns().add(UpsertColumn.of(primaryKey.getName(), false));

        for (List<Object> values : parameter.getValues()) {
            List<Object> newValues = new ArrayList<>(values.size() + 1);
            newValues.addAll(values);
            newValues.add(null);
            copy.getValues().add(newValues);
        }
        return copy;
    }

    private static boolean hasColumn(UpsertOperatorParameter parameter, RDBColumnMetadata column) {
        for (UpsertColumn upsertColumn : parameter.getColumns()) {
            if (column.equalsNameOrAlias(upsertColumn.getColumn())) {
                return true;
            }
        }
        return false;
    }
}
