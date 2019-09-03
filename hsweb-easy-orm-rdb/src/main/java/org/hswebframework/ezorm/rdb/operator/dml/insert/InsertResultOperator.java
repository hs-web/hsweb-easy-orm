package org.hswebframework.ezorm.rdb.operator.dml.insert;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.AsyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import reactor.core.publisher.Mono;

import java.util.concurrent.CompletionStage;

@AllArgsConstructor(staticName = "of")
public class InsertResultOperator implements ResultOperator<Integer, Integer> {

    private RDBTableMetadata table;

    private SqlRequest sql;

    @Override
    public Integer sync() {
        return table.<SyncSqlExecutor>findFeature(SyncSqlExecutor.id)
                .map(executor -> executor.update(sql))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported SyncSqlExecutor"));
    }

    @Override
    public CompletionStage<Integer> async() {
        return table.<AsyncSqlExecutor>findFeature(AsyncSqlExecutor.id)
                .map(executor -> executor.update(sql))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported AsyncSqlExecutor"));
    }

    @Override
    public Mono<Integer> reactive() {
        return table.<ReactiveSqlExecutor>findFeature(ReactiveSqlExecutor.id)
                .map(executor -> executor.update(Mono.just(sql)))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported AsyncSqlExecutor"));
    }
}
