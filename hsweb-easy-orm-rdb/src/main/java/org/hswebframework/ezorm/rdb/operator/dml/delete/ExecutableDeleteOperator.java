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
        return table.findFeatureNow(DeleteSqlBuilder.ID).build(getParameter());
    }

    @Override
    public DeleteResultOperator execute() {
        return DefaultDeleteResultOperator.of(table, this::getSql);
    }
}
