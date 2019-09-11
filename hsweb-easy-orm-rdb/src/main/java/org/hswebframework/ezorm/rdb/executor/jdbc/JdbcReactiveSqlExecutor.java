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

import java.sql.Connection;
import java.util.stream.Collectors;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;

@Slf4j
public abstract class JdbcReactiveSqlExecutor extends JdbcSqlExecutor implements ReactiveSqlExecutor {

    public JdbcReactiveSqlExecutor() {
        super(log);
    }

    public abstract Mono<Connection> getConnection(SqlRequest sqlRequest);

    public abstract void releaseConnection(Connection connection, SqlRequest sqlRequest);

    @Override
    public Mono<Integer> update(Publisher<SqlRequest> request) {

        return Mono.defer(() -> toFlux(request)
                .flatMap(sqlRequest -> getConnection(sqlRequest)
                        .flatMap(connection -> Mono.fromSupplier(() -> doUpdate(connection, sqlRequest))
                                .doFinally((type) -> releaseConnection(connection, sqlRequest))))
                .collect(Collectors.summingInt(Integer::intValue)));

    }

    @Override
    public Mono<Void> execute(Publisher<SqlRequest> request) {

        return Mono.defer(() -> toFlux(request)
                .flatMap(sqlRequest -> getConnection(sqlRequest)
                        .flatMap(connection ->
                                Mono.<Void>fromSupplier(() -> {
                                    doExecute(connection, sqlRequest);
                                    return null;
                                }).doFinally(type -> releaseConnection(connection, sqlRequest)))).then());
    }

    @Override
    public <E> Flux<E> select(Publisher<SqlRequest> request, ResultWrapper<E, ?> wrapper) {

        return Flux.create(sink -> {
            Disposable disposable = toFlux(request)
                    .doFinally(type -> sink.complete())
                    .subscribe(sqlRequest -> getConnection(sqlRequest)
                            .subscribe(connection -> {
                                doSelect(connection, sqlRequest, consumer(wrapper, sink::next));
                                releaseConnection(connection, sqlRequest);
                            }));

            sink.onCancel(disposable)
                    .onDispose(disposable);
        });
    }

    protected Flux<SqlRequest> toFlux(Publisher<SqlRequest> request) {
        return Flux.from(request);
    }
}
