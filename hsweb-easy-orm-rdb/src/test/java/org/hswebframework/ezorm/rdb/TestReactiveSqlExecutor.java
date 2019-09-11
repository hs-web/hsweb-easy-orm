package org.hswebframework.ezorm.rdb;

import io.r2dbc.spi.Connection;
import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.r2dbc.R2dbcReactiveSqlExecutor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

public class TestReactiveSqlExecutor extends R2dbcReactiveSqlExecutor {

    private R2dbcConnectionProvider provider;

    private String symbol;

    public TestReactiveSqlExecutor(String symbol, R2dbcConnectionProvider provider) {
        this.provider = provider;
        this.symbol = symbol;
    }

    public TestReactiveSqlExecutor(R2dbcConnectionProvider provider) {
        this(null, provider);
    }

    @Override
    protected Mono<Connection> getConnection() {
        return provider.getConnection();
    }

    @Override
    protected void releaseConnection(SignalType type, Connection connection) {

        provider.releaseConnection(connection);
    }

    @Override
    protected String getBindSymbol() {
        if (symbol != null) {
            return symbol;
        }
        return super.getBindSymbol();
    }
}
