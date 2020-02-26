package org.hswebframework.ezorm.rdb;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.jdbc.JdbcReactiveSqlExecutor;
import reactor.core.publisher.Mono;

import java.sql.Connection;

@AllArgsConstructor
public class TestJdbcReactiveSqlExecutor extends JdbcReactiveSqlExecutor {

    private ConnectionProvider provider;

    @Override
    public Mono<Connection> getConnection(SqlRequest sqlRequest) {
        return Mono.fromSupplier(provider::getConnection);
    }

    @Override
    public void releaseConnection(Connection connection, SqlRequest sqlRequest) {
        provider.releaseConnect(connection);
    }
}
