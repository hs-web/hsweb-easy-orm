package org.hswebframework.ezorm.rdb.executor.jdbc;

import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.context.Context;

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
        return Mono
            .deferContextual(ctx -> getConnection()
                .flatMap(connection -> toFlux(request)
                    .map(sql -> doUpdate(ctx.getOrDefault(Logger.class, log), connection, sql))
                    .reduce(Math::addExact))
                .defaultIfEmpty(0));

    }

    @Override
    public Mono<Void> execute(Publisher<SqlRequest> request) {

        return Mono
            .deferContextual(ctx -> getConnection()
                .flatMap(connection -> toFlux(request)
                    .doOnNext(sql -> doExecute(ctx.getOrDefault(Logger.class, log), connection, sql))
                    .then()));
    }

    @Override
    public <E> Flux<E> select(Publisher<SqlRequest> request, ResultWrapper<E, ?> wrapper) {
        return Flux
            .deferContextual(ctx -> {
                Logger logger = ctx.getOrDefault(Logger.class, log);
                return Flux
                    .create(sink -> {
                        @SuppressWarnings("all")
                        Disposable disposable = getConnection()
                            .flatMap(connection -> toFlux(request)
                                .doOnNext(sql -> this
                                    .doSelect(
                                        logger,
                                        connection,
                                        sql,
                                        consumer(wrapper, sink::next),
                                        t -> sink.isCancelled()))
                                .then())
                            .subscribe((ignore) -> sink.complete(),
                                       sink::error,
                                       sink::complete,
                                       Context.of(sink.contextView()));

                        sink.onCancel(disposable)
                            .onDispose(disposable);
                    });
            });

    }

    protected Flux<SqlRequest> toFlux(Publisher<SqlRequest> request) {
        return Flux.from(request);
    }
}
