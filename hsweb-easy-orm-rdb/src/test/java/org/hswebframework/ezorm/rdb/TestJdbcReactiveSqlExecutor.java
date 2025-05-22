package org.hswebframework.ezorm.rdb;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.jdbc.JdbcReactiveSqlExecutor;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.sql.Connection;
import java.util.function.Function;

@AllArgsConstructor
public class TestJdbcReactiveSqlExecutor extends JdbcReactiveSqlExecutor {

    private final ConnectionProvider provider;

    @Override
    public Mono<Connection> getConnection() {
        return Mono
                .using(
                        provider::getConnection,
                        Mono::just,
                        provider::releaseConnect
                );
    }

    @Override
    protected <T> Flux<T> doInConnection(Function<Connection, Publisher<T>> handler) {
        return Flux
            .using(
                provider::getConnection,
                handler,
                provider::releaseConnect
            );
    }
}
