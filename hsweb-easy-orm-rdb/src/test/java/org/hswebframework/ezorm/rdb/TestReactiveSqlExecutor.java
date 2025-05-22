package org.hswebframework.ezorm.rdb;

import io.r2dbc.spi.Connection;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.executor.reactive.r2dbc.R2dbcReactiveSqlExecutor;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.util.function.Function;

@Slf4j
public class TestReactiveSqlExecutor extends R2dbcReactiveSqlExecutor {

    private R2dbcConnectionProvider provider;

    private String symbol;

    private int bindIndex = 1;

    public TestReactiveSqlExecutor(String symbol, R2dbcConnectionProvider provider) {
        this.provider = provider;
        this.symbol = symbol;
    }

    public TestReactiveSqlExecutor(String symbol, int bindIndex, R2dbcConnectionProvider provider) {
        this.symbol = symbol;
        this.bindIndex = bindIndex;
        this.provider = provider;
    }

    public TestReactiveSqlExecutor(R2dbcConnectionProvider provider) {
        this(null, provider);
    }

    @Override
    protected Mono<Connection> getConnection() {
        return provider.getConnection()
                       .doOnNext(connection -> log.debug("get connection {}", connection));
    }

    @Override
    protected <T> Flux<T> doInConnection(Function<Connection, Publisher<T>> handler) {
        return Flux.usingWhen(
            provider.getConnection(),
            handler,
            c -> Mono.fromRunnable(() -> provider.releaseConnection(c)));
    }

    @Override
    protected void releaseConnection(SignalType type, Connection connection) {
        log.debug("release connection {}", connection);
        provider.releaseConnection(connection);
    }

    @Override
    public int getBindFirstIndex() {
        return bindIndex;
    }

    @Override
    protected String getBindSymbol() {
        if (symbol != null) {
            return symbol;
        }
        return super.getBindSymbol();
    }
}
