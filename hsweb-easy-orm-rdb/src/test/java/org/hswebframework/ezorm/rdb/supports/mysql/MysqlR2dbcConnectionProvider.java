package org.hswebframework.ezorm.rdb.supports.mysql;

import io.r2dbc.spi.*;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;
import reactor.test.StepVerifier;

import java.net.URL;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.function.Supplier;

import static io.r2dbc.spi.ConnectionFactoryOptions.*;
import static org.hswebframework.ezorm.rdb.executor.SqlRequests.*;

public class MysqlR2dbcConnectionProvider implements R2dbcConnectionProvider {


    private final ConnectionFactory connectionFactory;

    @SneakyThrows
    public MysqlR2dbcConnectionProvider() {

        String username = System.getProperty("mysql.username", "root");
        String password = System.getProperty("mysql.password", "root");
        String url = System.getProperty("mysql.url", "127.0.0.1:13306");
        String db = System.getProperty("mysql.db", "ezorm");

        URL hostUrl = new URL("file://" + url);

        connectionFactory = ConnectionFactories.get(ConnectionFactoryOptions.builder()
                .option(DRIVER, "mysql")
                .option(HOST, hostUrl.getHost())
                .option(PORT, hostUrl.getPort())
                .option(USER, username)
                .option(PASSWORD, password)
                .option(DATABASE, db)
                .build());

    }

    @Override
    public Mono<Connection> getConnection() {
        return Mono.defer(() -> Mono.from(connectionFactory.create()));
    }

    @Override
    public void releaseConnection(Connection connection) {
        connection.close();
    }
}
