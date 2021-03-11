package org.hswebframework.ezorm.rdb.supports.mssql;

import io.r2dbc.mssql.MssqlConnectionFactory;
import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactoryOptions;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.R2dbcConnectionProvider;
import reactor.core.publisher.Mono;

import java.net.URL;
import java.util.function.Supplier;

import static io.r2dbc.spi.ConnectionFactoryOptions.*;

public class MSSQLR2dbcConnectionProvider implements R2dbcConnectionProvider {


    Supplier<Mono<Connection>> connectionSupplier;

    @SneakyThrows
    public MSSQLR2dbcConnectionProvider() {

        String username = System.getProperty("mssql.username", "sa");
        String password = System.getProperty("mssql.password", "ezorm@PasswOrd");
        String url = System.getProperty("mssql.url", "127.0.0.1:11433");

        URL hostUrl = new URL("file://" + url);

        MssqlConnectionFactory connectionFactory = (MssqlConnectionFactory) ConnectionFactories.get(ConnectionFactoryOptions.builder()
                .option(DRIVER, "mssql")
                .option(HOST, hostUrl.getHost())
                .option(PORT, hostUrl.getPort())
                .option(USER, username)
                .option(PASSWORD, password)
//                .option(DATABASE, "dbo")
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
