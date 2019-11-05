package org.hswebframework.ezorm.rdb.operator.dml.update;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.update.UpdateSqlBuilder;

@AllArgsConstructor(staticName = "of")
public class ExecutableUpdateOperator extends BuildParameterUpdateOperator {

    private RDBTableMetadata table;

    @Override
    public SqlRequest getSql() {
        return table.<UpdateSqlBuilder>findFeature(UpdateSqlBuilder.ID_VALUE)
                .map(builder -> builder.build(getParameter()))
                .orElseThrow(() -> new UnsupportedOperationException("Unsupported UpdateSqlBuilder"));
    }

    @Override
    public UpdateResultOperator execute() {
        return UpdateResultOperator.of(table, getSql());
    }
}
