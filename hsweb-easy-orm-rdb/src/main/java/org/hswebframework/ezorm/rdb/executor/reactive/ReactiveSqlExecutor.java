package org.hswebframework.ezorm.rdb.executor.reactive;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface ReactiveSqlExecutor {

    Mono<Integer> update(Publisher<SqlRequest> request);

    Mono<Void> execute(Publisher<SqlRequest> request);

    <E> Flux<E> select(Publisher<SqlRequest> request, ResultWrapper<E, E> wrapper);

}
