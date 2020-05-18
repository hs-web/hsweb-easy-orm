package org.hswebframework.ezorm.rdb.executor.reactive;

import lombok.AllArgsConstructor;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.concurrent.TimeUnit;

@AllArgsConstructor(staticName = "of")
public class ReactiveSyncSqlExecutor implements SyncSqlExecutor {

    private final ReactiveSqlExecutor sqlExecutor;

    @Override
    public int update(SqlRequest request) {
        return sqlExecutor
                .update(Mono.just(request))
                .blockOptional(Duration.ofMinutes(2))
                .orElse(0);
    }

    @Override
    public void execute(SqlRequest request) {
        sqlExecutor
                .execute(Mono.just(request))
                .block(Duration.ofMinutes(2));
    }

    @Override
    @SneakyThrows
    public <T, R> R select(SqlRequest request, ResultWrapper<T, R> wrapper) {

        sqlExecutor.select(Mono.just(request), wrapper)
                .collectList()
                .toFuture()
                .get(10, TimeUnit.MINUTES);

        return wrapper.getResult();
    }
}
