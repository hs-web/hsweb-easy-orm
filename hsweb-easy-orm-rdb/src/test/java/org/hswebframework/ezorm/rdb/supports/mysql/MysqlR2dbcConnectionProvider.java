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
        ReactiveSqlExecutor sqlExecutor = new TestReactiveSqlExecutor(new MysqlR2dbcConnectionProvider()) {
            @Override
            protected void releaseConnection(SignalType type, Connection connection) {
                super.releaseConnection(type, connection);
            }

            @Override
            protected Flux<Result> doExecute(Flux<SqlRequest> sqlRequestFlux) {

                return sqlRequestFlux.concatMap(sqlRequest ->
                        this.getConnection().flux()
                                .flatMap(connection ->
                                        this.doExecute(connection, sqlRequest)
                                                .doFinally(si -> releaseConnection(si, connection))));

            }

            @Override
            protected void bindNull(Statement statement, int index, Class type) {
                statement.bindNull(index, type);
            }

            @Override
            protected void bind(Statement statement, int index, Object value) {
                if (value instanceof LocalDateTime) {
                    value = Date.from(((LocalDateTime) value).atZone(ZoneOffset.UTC).toInstant());
                }
                statement.bind(index, value);
            }

            @Override
            protected SqlRequest convertRequest(SqlRequest sqlRequest) {
                return sqlRequest;
            }
        };

        Mono.just("drop table if exists ezorm.entity_test_table")
                .map(SqlRequests::of)
                .as(sqlExecutor::execute)
                .block();

        Mono.just("create table ezorm.entity_test_table ( `create_time` datetime(6) , `balance` bigint , `name` varchar(255) not null , `state` tinyint not null , `id` varchar(32) not null primary key )")
                .map(SqlRequests::of)
                .as(sqlExecutor::execute)
                .block();

        Mono.just("insert into ezorm.entity_test_table ( `balance` , `create_time` , `name` , `id` , `state` ) values  ( ? , ? , ? , ? , ? )")
                .map(sql -> of(sql, 1000L, new Date(), "test", "test_id", (byte) 1))
                .as(sqlExecutor::update)
                .as(StepVerifier::create)
                .expectNext(1)
                .verifyComplete();

        Mono.just("update ezorm.entity_test_table set `name` = ? where `id` = ?")
                .map(sql -> of(sql, "1234", "test_id"))
                .as(sqlExecutor::update)
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
