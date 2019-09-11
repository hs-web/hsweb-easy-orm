package org.hswebframework.ezorm.rdb.operator.ddl;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.AsyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Mono;

import java.util.concurrent.CompletionStage;

@AllArgsConstructor(staticName = "of")
public class TableDDLResultOperator implements ResultOperator<Boolean, Boolean> {

    private RDBSchemaMetadata schema;

    private SqlRequest sqlRequest;

    private Runnable whenCompleted;

    @Override
    public Boolean sync() {
        return schema.<SyncSqlExecutor>findFeature(SyncSqlExecutor.id)
                .map(sqlExecutor -> {
                    sqlExecutor.execute(sqlRequest);
                    whenCompleted.run();
                    return true;
                })
                .orElseThrow(() -> new UnsupportedOperationException("Unsupported SyncSqlExecutor"));

    }

    @Override
    public CompletionStage<Boolean> async() {
        return schema.<AsyncSqlExecutor>findFeature(AsyncSqlExecutor.id)
                .map(sqlExecutor -> sqlExecutor.execute(sqlRequest)
                        .thenApply(__ -> {
                            whenCompleted.run();
                            return true;
                        }))
                .orElseThrow(() -> new UnsupportedOperationException("Unsupported AsyncSqlExecutor"));
    }

    @Override
    public Mono<Boolean> reactive() {
        return schema.<ReactiveSqlExecutor>findFeature(ReactiveSqlExecutor.id)
                .map(sqlExecutor -> sqlExecutor
                        .execute(Mono.just(sqlRequest))
                        .doOnSuccess(__->whenCompleted.run())
                        .thenReturn(true))
                .orElseThrow(() -> new UnsupportedOperationException("Unsupported ReactiveSqlExecutor"));
    }

}
