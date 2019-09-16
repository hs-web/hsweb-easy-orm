package org.hswebframework.ezorm.rdb.executor.reactive.r2dbc;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.Result;
import io.r2dbc.spi.Statement;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.CastUtil;
import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.DefaultColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import reactor.core.publisher.*;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static org.hswebframework.ezorm.rdb.utils.SqlUtils.*;

@Slf4j
public abstract class R2dbcReactiveSqlExecutor implements ReactiveSqlExecutor {

    @Getter
    @Setter
    private Logger logger = log;

    protected abstract Mono<Connection> getConnection();

    protected abstract void releaseConnection(SignalType type, Connection connection);

    enum Interrupted {
        instance;

        static boolean nonInterrupted(Object o) {
            return o != instance;
        }
    }

    protected Flux<Result> execute(Connection connection,
                                   SqlRequest request) {

        return Flux.just(prepareStatement(connection.createStatement(request.getSql()), request))
                .flatMap(Statement::execute)
                .map(Result.class::cast)
                .doOnSubscribe(subscription -> printSql(logger, request));
    }

    @Override
    public Mono<Integer> update(Publisher<SqlRequest> request) {
        return Mono.defer(() -> getConnection().flux()
                .flatMap(connection ->
                        this.toFlux(request)
                                .flatMap(sqlRequest -> this.execute(connection, sqlRequest))
                                .flatMap(result -> Mono.from(result.getRowsUpdated()).defaultIfEmpty(0))
                                .doFinally(type -> releaseConnection(type, connection)))
                .collect(Collectors.summingInt(Integer::intValue)));
    }

    @Override
    public Mono<Void> execute(Publisher<SqlRequest> request) {
        return Mono.defer(() -> Mono.from(getConnection().flux()
                .flatMap(connection ->
                        this.toFlux(request)
                                .flatMap(sqlRequest -> this.execute(connection, sqlRequest))
                                .flatMap(__ -> Mono.<Void>empty())
                                .doFinally(type -> releaseConnection(type, connection))
                )));
    }

    @Override
    public <E> Flux<E> select(Publisher<SqlRequest> request, ResultWrapper<E, ?> wrapper) {
        return Flux.defer(() -> getConnection().flux()
                .flatMap(connection ->
                        this.toFlux(request)
                                .flatMap(sqlRequest -> this.execute(connection, sqlRequest))
                                .flatMap(result -> result.map((row, meta) -> {
                                    List<String> columns = new ArrayList<>(meta.getColumnNames());
                                    wrapper.beforeWrap(() -> columns);
                                    E e = wrapper.newRowInstance();
                                    for (int i = 0, len = columns.size(); i < len; i++) {
                                        DefaultColumnWrapperContext<E> context = new DefaultColumnWrapperContext<>( i, columns.get(i), row.get(i), e);
                                        wrapper.wrapColumn(context);
                                        e = context.getInstance();
                                    }
                                    if (!wrapper.completedWrapRow(e)) {
                                        return Interrupted.instance;
                                    }
                                    return e;
                                }))
                                .takeWhile(Interrupted::nonInterrupted)
                                .map(CastUtil::<E>cast)
                                .doOnCancel(wrapper::completedWrap)
                                .doOnComplete(wrapper::completedWrap)
                                .doFinally(type -> releaseConnection(type, connection))
                ));
    }

    @SuppressWarnings("all")
    protected Flux<SqlRequest> toFlux(Publisher<SqlRequest> request) {

        return Flux.from(request)
                .flatMap(sql -> {
                    if (sql instanceof BatchSqlRequest) {
                        return Flux.concat(Flux.just(sql), Flux.fromIterable(((BatchSqlRequest) sql).getBatch()));
                    }
                    return Flux.just(sql);
                }).filter(SqlRequest::isNotEmpty)
                .map(this::convertRequest);
    }

    protected SqlRequest convertRequest(SqlRequest sqlRequest) {
        return R2dbcSqlRequest.of(getBindSymbol(), sqlRequest);
    }

    protected String getBindSymbol() {
        return "$";
    }

    protected Statement prepareStatement(Statement statement, SqlRequest request) {
        int index = 0;
        if (request.isEmpty() || request.getParameters() == null) {
            return statement;
        }
        for (Object parameter : request.getParameters()) {
            String symbol = getBindSymbol() + ++index;
            if (parameter == null) {
                statement.bindNull(symbol, Object.class);
            } else {
                statement.bind(symbol, parameter);
            }
        }
        return statement;
    }
}
