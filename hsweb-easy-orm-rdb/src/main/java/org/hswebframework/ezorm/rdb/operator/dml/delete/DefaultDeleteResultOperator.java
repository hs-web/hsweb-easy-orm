package org.hswebframework.ezorm.rdb.operator.dml.delete;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.AsyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import org.hswebframework.ezorm.rdb.utils.ExceptionUtils;
import reactor.core.publisher.Mono;

import java.util.concurrent.CompletionStage;
import java.util.function.Supplier;

@AllArgsConstructor(staticName = "of")
 class DefaultDeleteResultOperator implements DeleteResultOperator{

    private RDBTableMetadata table;

    private Supplier<SqlRequest> sql;

    @Override
    public Integer sync() {
        return ExceptionUtils.translation(() -> table.findFeature(SyncSqlExecutor.ID)
                .map(executor -> executor.update(sql.get()))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported SyncSqlExecutor")), table);
    }

    @Override
    public Mono<Integer> reactive() {
       return Mono.defer(()-> table.findFeature(ReactiveSqlExecutor.ID)
               .orElseThrow(() -> new UnsupportedOperationException("unsupported AsyncSqlExecutor"))
               .update(Mono.fromSupplier(sql))
               .onErrorMap(error -> ExceptionUtils.translation(table, error)));
    }

}
