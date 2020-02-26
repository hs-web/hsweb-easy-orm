package org.hswebframework.ezorm.rdb.operator.dml.insert;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.InsertSqlBuilder;

@AllArgsConstructor(staticName = "of")
public class ExecutableInsertOperator extends BuildParameterInsertOperator {

    private RDBTableMetadata table;

    @Override
    public SqlRequest getSql() {
        return table
                .findFeatureNow(InsertSqlBuilder.ID)
                .build(getParameter());
    }

    @Override
    public InsertResultOperator execute() {

        return DefaultInsertResultOperator.of(table, this::getSql);

    }
}
