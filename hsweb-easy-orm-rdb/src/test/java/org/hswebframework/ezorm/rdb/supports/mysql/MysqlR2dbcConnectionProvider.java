package org.hswebframework.ezorm.rdb.supports.mysql;

import io.r2dbc.spi.*;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.R2dbcConnectionProvider;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URL;
import java.util.function.Supplier;

import static io.r2dbc.spi.ConnectionFactoryOptions.*;

public class MysqlR2dbcConnectionProvider implements R2dbcConnectionProvider {


    private ConnectionFactory connectionFactory;

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

    @SneakyThrows
    public static void main(String[] args) {
        String username = System.getProperty("mysql.username", "root");
        String password = System.getProperty("mysql.password", "root");
        String url = System.getProperty("mysql.url", "127.0.0.1:13306");
        String db = System.getProperty("mysql.db", "ezorm");

        URL hostUrl = new URL("file://" + url);

        ConnectionFactory connectionFactory = ConnectionFactories.get(ConnectionFactoryOptions.builder()
                .option(DRIVER, "mysql")
                .option(HOST, hostUrl.getHost())
                .option(PORT, hostUrl.getPort())
                .option(USER, username)
                .option(PASSWORD, password)
                .option(DATABASE, db)
                .build());

        Mono.from(connectionFactory.create())
                .map(conn -> conn.createStatement("drop table if exists ezorm.r2dbc_test").execute())
                .flatMap(Mono::from)
                .block();

        Mono.from(connectionFactory.create())
                .map(conn -> conn.createStatement("create table ezorm.r2dbc_test(id bigint ,name varchar(32))").execute())
                .flatMap(Mono::from)
                .block();


        Mono.from(connectionFactory.create())
                .map(conn -> conn.createStatement("insert into ezorm.r2dbc_test values(?,?)")
                        .bind(0, 1)
                        .bind(1, "name")
                        .execute())
                .flatMap(Mono::from)
                .map(Result::getRowsUpdated)
                .flatMap(Mono::from)
                .as(StepVerifier::create)
                .expectNext(1)
                .verifyComplete();

        Mono.from(connectionFactory.create())
                .map(conn -> conn.createStatement("update ezorm.r2dbc_test set `name` = ? where `name` = ?")
                        .bind(0, "new-name")
                        .bind(1, "name")
                        .execute())
                .flatMap(Mono::from)
                .map(Result::getRowsUpdated)
                .flatMap(Mono::from)
                .as(StepVerifier::create)
                .expectNext(1)
                .verifyComplete();

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
