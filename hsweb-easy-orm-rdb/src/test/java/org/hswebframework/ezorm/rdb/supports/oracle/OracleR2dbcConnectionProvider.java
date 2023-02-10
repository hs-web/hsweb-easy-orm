package org.hswebframework.ezorm.rdb.supports.oracle;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.ConnectionFactoryOptions;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.mysql.Mysql57ConnectionProvider;
import reactor.core.publisher.Mono;

import java.net.URL;
import java.util.Queue;
import java.util.concurrent.LinkedBlockingQueue;

import static io.r2dbc.spi.ConnectionFactoryOptions.*;

public class OracleR2dbcConnectionProvider implements R2dbcConnectionProvider {

    private final ConnectionFactory connectionFactory;

    private Queue<Mono<Connection>> connections = new LinkedBlockingQueue<>();

    Mono<Connection> connectionSupplier;

    @SneakyThrows
    public OracleR2dbcConnectionProvider() {

        String username = System.getProperty("oracle.username", "system");
        String password = System.getProperty("oracle.password", "oracle");
        String url = System.getProperty("oracle.url", "127.0.0.1:" + OracleConnectionProvider.port);
        String db = System.getProperty("oracle.db", "orcl");

        URL hostUrl = new URL("file://" + url);

        connectionFactory = ConnectionFactories.get(ConnectionFactoryOptions
                                                            .builder()
                                                            .option(DRIVER, "oracle")
                                                            .option(HOST, hostUrl.getHost())
                                                            .option(PORT, hostUrl.getPort())
                                                            .option(USER, username)
                                                            .option(PASSWORD, password)
                                                            .option(DATABASE, db)
                                                            .build());

        connectionSupplier = Mono.from(connectionFactory.create());

        for (int i = 0; i < 10; i++) {
            connections.add(connectionSupplier.cache());
        }
    }

    @Override
    public Mono<Connection> getConnection() {
        Mono<Connection> conn = connections.poll();
        return conn == null ? connectionSupplier.cache() : conn;
    }

    @Override
    public void releaseConnection(Connection connection) {
        connections.add(Mono.just(connection));
    }
}
