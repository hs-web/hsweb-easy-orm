package org.hswebframework.ezorm.rdb.executor.reactive;

import lombok.AllArgsConstructor;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.time.Duration;
import java.util.concurrent.TimeUnit;

@AllArgsConstructor(staticName = "of")
public class ReactiveSyncSqlExecutor implements SyncSqlExecutor {

    private final ReactiveSqlExecutor sqlExecutor;

    @Override
    public int update(SqlRequest request) {
        return sqlExecutor
                .update(Mono.just(request))
                .publishOn(Schedulers.elastic())
                .blockOptional(Duration.ofSeconds(30))
                .orElse(0);
    }

    @Override
    public void execute(SqlRequest request) {
        sqlExecutor
                .execute(Mono.just(request))
                .publishOn(Schedulers.elastic())
                .block(Duration.ofSeconds(30));
    }

    @Override
    @SneakyThrows
    public <T, R> R select(SqlRequest request, ResultWrapper<T, R> wrapper) {
        sqlExecutor.select(Mono.just(request), wrapper)
                .collectList()
                .publishOn(Schedulers.elastic())
                .block(Duration.ofSeconds(30));

        return wrapper.getResult();
    }
}
