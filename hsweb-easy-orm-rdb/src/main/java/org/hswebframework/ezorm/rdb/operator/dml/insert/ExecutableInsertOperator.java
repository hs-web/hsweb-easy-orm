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
        return table.findFeature(InsertSqlBuilder.ID)
                .map(builder -> builder.build(getParameter()))
                .orElseThrow(() -> new UnsupportedOperationException("table [" + table.getFullName() + " unsupported InsertSqlBuilder]"));
    }

    @Override
    public InsertResultOperator execute() {

        return DefaultInsertResultOperator.of(table, this::getSql);

    }
}
