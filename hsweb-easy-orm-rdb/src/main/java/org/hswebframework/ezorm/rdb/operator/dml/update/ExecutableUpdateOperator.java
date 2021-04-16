package org.hswebframework.ezorm.rdb.operator.dml.update;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.update.UpdateSqlBuilder;

@AllArgsConstructor(staticName = "of")
public class ExecutableUpdateOperator extends BuildParameterUpdateOperator {

    private final RDBTableMetadata table;

    @Override
    public SqlRequest getSql() {
        return table.<UpdateSqlBuilder>findFeatureNow(UpdateSqlBuilder.ID_VALUE)
                .build(getParameter());
    }

    @Override
    public UpdateResultOperator execute() {
        return DefaultUpdateResultOperator.of(table, this::getSql);
    }
}
