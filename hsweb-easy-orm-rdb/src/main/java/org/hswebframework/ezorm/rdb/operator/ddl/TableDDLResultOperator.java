package org.hswebframework.ezorm.rdb.operator.ddl;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import org.hswebframework.ezorm.rdb.utils.ExceptionUtils;
import reactor.core.publisher.Mono;

@AllArgsConstructor(staticName = "of")
public class TableDDLResultOperator implements ResultOperator<Boolean, Boolean> {

    private RDBSchemaMetadata schema;

    private SqlRequest sqlRequest;

    private Runnable whenCompleted;

    @Override
    public Boolean sync() {
        return ExceptionUtils.translation(() -> schema.findFeature(SyncSqlExecutor.ID)
                .map(sqlExecutor -> {
                    sqlExecutor.execute(sqlRequest);
                    whenCompleted.run();
                    return true;
                })
                .orElseThrow(() -> new UnsupportedOperationException("Unsupported SyncSqlExecutor")), schema);

    }

    @Override
    public Mono<Boolean> reactive() {
        return schema.findFeatureNow(ReactiveSqlExecutor.ID)
                .execute(Mono.just(sqlRequest))
                .doOnSuccess(__ -> whenCompleted.run())
                .thenReturn(true)
                .onErrorMap(error -> ExceptionUtils.translation(schema, error))
                ;
    }

}
