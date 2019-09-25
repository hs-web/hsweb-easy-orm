package org.hswebframework.ezorm.rdb.operator.dml.delete;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.delete.DeleteSqlBuilder;

@AllArgsConstructor(staticName = "of")
public class ExecutableDeleteOperator extends BuildParameterDeleteOperator {

    private RDBTableMetadata table;

    @Override
    public SqlRequest getSql() {
        return table.findFeature(DeleteSqlBuilder.ID)
                .map(builder -> builder.build(getParameter()))
                .orElseThrow(() -> new UnsupportedOperationException("Unsupported DeleteSqlBuilder"));
    }

    @Override
    public DeleteResultOperator execute() {
        return DeleteResultOperator.of(table, getSql());
    }
}
