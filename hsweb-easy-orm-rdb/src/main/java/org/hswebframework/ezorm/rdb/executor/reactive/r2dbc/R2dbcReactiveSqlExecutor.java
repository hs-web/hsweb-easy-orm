package org.hswebframework.ezorm.rdb.executor.reactive.r2dbc;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.Result;
import io.r2dbc.spi.Statement;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.DefaultColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.utils.SqlUtils;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import reactor.core.publisher.EmitterProcessor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.hswebframework.ezorm.rdb.utils.SqlUtils.*;

@Slf4j
public abstract class R2dbcReactiveSqlExecutor implements ReactiveSqlExecutor {

    @Getter
    @Setter
    private Logger logger = log;

    protected abstract Mono<Connection> getConnection();

    protected abstract void releaseConnect(SignalType type, Connection connection);


    @SuppressWarnings("all")
    protected <T> Publisher<T> doUpdate(Connection connection, SqlRequest request, Function<Result, Publisher<T>> mapper) {

        EmitterProcessor<Result> processor = EmitterProcessor.create();
        Flux<T> resultFlux = processor.flatMap(mapper);

        printSql(logger, request);
        Statement statement = connection.createStatement(request.getSql());
        prepareStatement(statement, request);

        statement.execute()
                .subscribe(processor);

        return resultFlux;
    }

    @SuppressWarnings("all")
    protected <E> Publisher<E> doSelect(Connection connection, SqlRequest request, ResultWrapper<E, ?> wrapper) {
        EmitterProcessor<Result> processor = EmitterProcessor.create();
        Flux<E> flux = processor.flatMap(result ->
                result.map((row, meta) -> {
                    List<String> columns = new ArrayList<>(meta.getColumnNames());

                    wrapper.beforeWrap(() -> columns);
                    E e = wrapper.newRowInstance();
                    for (int i = 0, len = columns.size(); i < len; i++) {
                        wrapper.wrapColumn(new DefaultColumnWrapperContext<>(-1, i, columns.get(i), row.get(i), e));
                    }
                    if (!wrapper.completedWrapRow(-1, e)) {
                        processor.dispose();
                    }
                    return e;
                })).doOnComplete(wrapper::completedWrap);

        printSql(logger, request);
        Statement statement = connection.createStatement(request.getSql());

        prepareStatement(statement, request);

        statement.execute().subscribe(processor);

        return flux;

    }


    @Override
    public Mono<Integer> update(Publisher<SqlRequest> request) {
        return Mono.defer(() -> getConnection()
                .flux()
                .flatMap(connection -> this.toFlux(request)
                        .flatMap(sqlRequest -> this.doUpdate(connection, sqlRequest, Result::getRowsUpdated))
                        .doFinally(type -> releaseConnect(type, connection))
                )
                .collect(Collectors.summingInt(Integer::intValue)));
    }

    @Override
    public Mono<Void> execute(Publisher<SqlRequest> request) {
        return Mono.defer(() -> Mono.from(getConnection()
                .flux()
                .flatMap(connection -> this.toFlux(request)
                        .flatMap(sqlRequest -> this.doUpdate(connection, sqlRequest, (r) -> Mono.<Void>empty()))
                        .doFinally(type -> releaseConnect(type, connection))
                )));
    }

    @Override
    public <E> Flux<E> select(Publisher<SqlRequest> request, ResultWrapper<E, ?> wrapper) {
        return Flux.defer(() -> getConnection()
                .flux()
                .flatMap(connection -> this.toFlux(request)
                        .flatMap(sqlRequest -> this.doSelect(connection, sqlRequest, wrapper))
                        .doFinally(type -> releaseConnect(type, connection))
                ));
    }

    @SuppressWarnings("all")
    protected Flux<SqlRequest> toFlux(Publisher<SqlRequest> request) {
        Flux<SqlRequest> flux;
        if (request instanceof Mono) {
            Mono<SqlRequest> mono = ((Mono<SqlRequest>) request);
            flux = mono.flux();
        } else if (request instanceof Flux) {
            flux = ((Flux<SqlRequest>) request);
        } else {
            return Flux.error(new UnsupportedOperationException("unsupported request type:" + request.getClass()));
        }
        return flux.flatMap(sql -> {
            if (sql instanceof BatchSqlRequest) {

                return Flux.merge(Flux.just(sql), Flux.fromIterable(((BatchSqlRequest) sql).getBatch()));
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

    protected void prepareStatement(Statement statement, SqlRequest request) {
        int index = 0;
        if(request.isEmpty()||request.getParameters()==null){
            return;
        }
        for (Object parameter : request.getParameters()) {
            String symbol = getBindSymbol() + ++index;
            if (parameter == null) {
                statement.bindNull(symbol, Object.class);
            } else {
                statement.bind(symbol, parameter);
            }
        }
    }
}
