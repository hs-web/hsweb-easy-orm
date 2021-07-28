package org.hswebframework.ezorm.rdb;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.jdbc.JdbcReactiveSqlExecutor;
import reactor.core.publisher.Mono;

import java.sql.Connection;

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


}
