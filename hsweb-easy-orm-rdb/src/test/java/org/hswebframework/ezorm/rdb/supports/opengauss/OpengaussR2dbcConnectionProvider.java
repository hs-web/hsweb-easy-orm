package org.hswebframework.ezorm.rdb.supports.opengauss;

import io.r2dbc.postgresql.PostgresqlConnectionFactory;
import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactoryOptions;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.Containers;
import org.hswebframework.ezorm.rdb.R2dbcConnectionProvider;
import org.junit.Assert;
import org.postgresql.Driver;
import org.testcontainers.containers.GenericContainer;
import reactor.core.publisher.Mono;

import java.net.URL;
import java.util.function.Supplier;

import static io.r2dbc.spi.ConnectionFactoryOptions.*;

public class OpengaussR2dbcConnectionProvider implements R2dbcConnectionProvider {


    Supplier<Mono<Connection>> connectionSupplier;

    static {
        OpengaussConnectionProvider.load();
    }

    @SneakyThrows
    public OpengaussR2dbcConnectionProvider() {

        String username = System.getProperty("gauss.username", "gaussdb");
        String password = System.getProperty("gauss.password", "Admin@1234Hs");
        String url = System.getProperty("gauss.url", "127.0.0.1:" + OpengaussConnectionProvider.port);
        String db = System.getProperty("gauss.db", "postgres");

        URL hostUrl = new URL("file://" + url);

        PostgresqlConnectionFactory connectionFactory = (PostgresqlConnectionFactory) ConnectionFactories.get(ConnectionFactoryOptions.builder()
                .option(DRIVER, "postgresql")
                .option(HOST, hostUrl.getHost())  // file, mem
                .option(PORT, hostUrl.getPort())  // file, mem
                .option(USER, username)
                .option(PASSWORD, password)
                .option(DATABASE, db)
                .build());
        connectionSupplier = () -> connectionFactory.create().map(Connection.class::cast);
    }

    @Override
    public Mono<Connection> getConnection() {
        return connectionSupplier.get();
    }

    @Override
    public void releaseConnection(Connection connection) {
        connection.close();
    }
}
