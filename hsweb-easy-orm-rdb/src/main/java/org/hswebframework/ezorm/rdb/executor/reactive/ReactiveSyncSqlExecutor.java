package org.hswebframework.ezorm.rdb.executor.reactive;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import reactor.core.publisher.Mono;

@AllArgsConstructor(staticName = "of")
public class ReactiveSyncSqlExecutor implements SyncSqlExecutor {

    private ReactiveSqlExecutor sqlExecutor;

    @Override
    public int update(SqlRequest request) {
        return sqlExecutor
                .update(Mono.just(request))
                .blockOptional()
                .orElse(0);
    }

    @Override
    public void execute(SqlRequest request) {
        sqlExecutor.execute(Mono.just(request))
                .block();
    }

    @Override
    public <T, R> R select(SqlRequest request, ResultWrapper<T, R> wrapper) {

        sqlExecutor.select(Mono.just(request), wrapper).collectList().block();

        return wrapper.getResult();
    }
}
