package org.hswebframework.ezorm.rdb.executor.jdbc;

import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.reactivestreams.Publisher;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.sql.Connection;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.consumer;

@Slf4j
public abstract class JdbcReactiveSqlExecutor extends JdbcSqlExecutor implements ReactiveSqlExecutor {

    public JdbcReactiveSqlExecutor() {
        super(log);
    }

    public abstract Mono<Connection> getConnection();

    @Override
    public Mono<Integer> update(Publisher<SqlRequest> request) {

        return getConnection()
                .flatMap(connection -> toFlux(request)
                        .map(sql -> doUpdate(connection, sql))
                        .reduce(Math::addExact))
                .defaultIfEmpty(0);

    }

    @Override
    public Mono<Void> execute(Publisher<SqlRequest> request) {
        return getConnection()
                .flatMap(connection -> toFlux(request)
                        .doOnNext(sql -> doExecute(connection, sql))
                        .then());
    }

    @Override
    public <E> Flux<E> select(Publisher<SqlRequest> request, ResultWrapper<E, ?> wrapper) {
        return Flux
                .create(sink -> {
                    Disposable disposable = getConnection()
                            .flatMap(connection -> toFlux(request)
                                    .doOnNext(sql -> doSelect(connection, sql, consumer(wrapper, sink::next)))
                                    .then())
                            .doOnError(sink::error)
                            .subscriberContext(sink.currentContext())
                            .doOnSuccess(ignore -> sink.complete())
                            .subscribe();

                    sink.onCancel(disposable)
                        .onDispose(disposable);
                });
    }

    protected Flux<SqlRequest> toFlux(Publisher<SqlRequest> request) {
        return Flux.from(request);
    }
}
