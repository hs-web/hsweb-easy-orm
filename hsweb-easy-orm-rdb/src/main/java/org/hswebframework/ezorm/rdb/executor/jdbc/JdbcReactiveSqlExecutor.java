package org.hswebframework.ezorm.rdb.executor.jdbc;

import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import reactor.core.Disposable;
import reactor.core.Disposables;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.util.context.Context;

import java.sql.Connection;
import java.util.function.Function;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.consumer;

@Slf4j
public abstract class JdbcReactiveSqlExecutor extends JdbcSqlExecutor implements ReactiveSqlExecutor {

    public JdbcReactiveSqlExecutor() {
        super(log);
    }

    @Deprecated
    public abstract Mono<Connection> getConnection();

    protected <T> Flux<T> doInConnection(Function<Connection, Publisher<T>> handler) {
        return getConnection()
            .flatMapMany(handler);
    }

    @Override
    public Mono<Integer> update(Publisher<SqlRequest> request) {
        return Flux
            .deferContextual(
                ctx ->
                    doInConnection(connection -> this
                        .toFlux(request)
                        .map(sql -> doUpdate(ctx.getOrDefault(Logger.class, log), connection, sql))
                        .reduce(Math::addExact)))
            .last(0)
            // 使用弹性线程池来执行jdbc操作
            .subscribeOn(Schedulers.boundedElastic())
            // 下游切换为parallel线程池,弹性线程池数据不可控，在虚拟线程等场景下，可能影响下游基于ThreadLocal等场景的缓存性能。
            .publishOn(Schedulers.parallel());

    }

    @Override
    public Mono<Void> execute(Publisher<SqlRequest> request) {

        return Flux
            .deferContextual(
                ctx ->
                    doInConnection(connection -> this
                        .toFlux(request)
                        .doOnNext(sql -> doExecute(ctx.getOrDefault(Logger.class, log), connection, sql))))
            .subscribeOn(Schedulers.boundedElastic())
            .publishOn(Schedulers.parallel())
            .then();
    }

    @Override
    public <E> Flux<E> select(Publisher<SqlRequest> request, ResultWrapper<E, ?> wrapper) {
        return Flux
            .<E>deferContextual(ctx -> {
                Logger logger = ctx.getOrDefault(Logger.class, log);
                return Flux
                    .create(sink -> {
                        Disposable.Composite disposable = Disposables.composite();

                        @SuppressWarnings("all")
                        Disposable queryDisposable = this
                            .doInConnection(connection -> {
                                return toFlux(request)
                                    .doOnNext(sql -> this
                                        .doSelect(
                                            logger,
                                            connection,
                                            sql,
                                            consumer(wrapper, sink::next),
                                            disposable))
                                    .then();
                            })
                            .subscribeOn(Schedulers.boundedElastic())
                            .subscribe((ignore) -> sink.complete(),
                                       sink::error,
                                       sink::complete,
                                       Context.of(sink.contextView()));

                        disposable.add(queryDisposable);

                        sink
                            .onDispose(disposable);
                    });
            })
            .publishOn(Schedulers.parallel());

    }

    protected Flux<SqlRequest> toFlux(Publisher<SqlRequest> request) {
        return Flux.from(request);
    }
}
