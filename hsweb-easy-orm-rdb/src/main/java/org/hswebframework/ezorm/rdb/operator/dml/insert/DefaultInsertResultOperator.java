package org.hswebframework.ezorm.rdb.operator.dml.insert;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.utils.ExceptionUtils;
import reactor.core.publisher.Mono;

import java.util.function.Supplier;

@AllArgsConstructor(staticName = "of")
public class DefaultInsertResultOperator implements InsertResultOperator {

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
        return table.findFeature(ReactiveSqlExecutor.ID)
                .orElseThrow(() -> new UnsupportedOperationException("unsupported ReactiveSqlExecutor"))
                .update(Mono.fromSupplier(sql))
                .onErrorMap((err) -> ExceptionUtils.translation(table, err));
    }
}
