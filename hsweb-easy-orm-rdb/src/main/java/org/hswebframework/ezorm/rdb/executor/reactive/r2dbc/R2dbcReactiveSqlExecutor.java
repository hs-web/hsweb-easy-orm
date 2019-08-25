package org.hswebframework.ezorm.rdb.executor.reactive.r2dbc;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public class R2dbcReactiveSqlExecutor implements ReactiveSqlExecutor {


//    protected Mono<Connection>

    @Override
    public Mono<Integer> update(Publisher<SqlRequest> request) {
        return null;
    }

    @Override
    public Mono<Void> execute(Publisher<SqlRequest> request) {
        return null;
    }

    @Override
    public <E> Flux<E> select(Publisher<SqlRequest> request, ResultWrapper<E, E> wrapper) {
        return null;
    }
}
