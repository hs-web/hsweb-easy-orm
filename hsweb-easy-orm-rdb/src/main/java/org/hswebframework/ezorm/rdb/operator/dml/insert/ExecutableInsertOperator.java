package org.hswebframework.ezorm.rdb.operator.dml.insert;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.AsyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.InsertSqlBuilder;
import reactor.core.publisher.Mono;

import java.util.concurrent.CompletionStage;

@AllArgsConstructor(staticName = "of")
public class ExecutableInsertOperator extends BuildParameterInsertOperator {

    private RDBTableMetadata table;

    @Override
    public SqlRequest getSql() {
        return table.<InsertSqlBuilder>findFeature(InsertSqlBuilder.id)
                .map(builder -> builder.build(getParameter()))
                .orElseThrow(() -> new UnsupportedOperationException("table [" + table.getFullName() + " unsupported InsertSqlBuilder]"));
    }

    @Override
    public InsertResultOperator execute() {

        return InsertResultOperator.of(table,getSql());

    }
}
