package org.hswebframework.ezorm.rdb.executor.jdbc;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.junit.Before;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.sql.Connection;
import java.sql.DriverManager;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.*;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;

public class JdbcReactiveSqlExecutorTest {

    JdbcReactiveSqlExecutor sqlExecutor;

    @Before
    @SneakyThrows
    public void init() {
        Class.forName("org.h2.Driver");
        Connection connection = DriverManager.getConnection("jdbc:h2:mem:hsweb", "sa", "");

        sqlExecutor = new JdbcReactiveSqlExecutor() {
            @Override
            public Mono<Connection> getConnection(SqlRequest sqlRequest) {
                return Mono.just(connection);
            }

            @Override
            public void releaseConnection(Connection connection, SqlRequest sqlRequest) {

            }
        };
    }

    @Test
    public void testExecute() {

        Mono<Void> ddl = sqlExecutor.execute(Mono.just(of("create table test( id varchar(32) primary key )")))
                .doOnError(Throwable::printStackTrace);

        StepVerifier.create(ddl)
                .verifyComplete();

        Mono<Integer> counter = sqlExecutor
                .update(Flux.range(0, 10)
                        .doOnNext(i -> System.out.println())
                        .map(num -> prepare("insert into test (id) values (?) ", num)))
                .doOnError(Throwable::printStackTrace);

        StepVerifier.create(counter)
                .expectNext(10)
                .verifyComplete();

        Mono<Long> data = sqlExecutor.select(Mono.just(of("select * from test")), map())
                .doOnError(Throwable::printStackTrace)
                .map(map -> map.get("ID"))
                .count();

        StepVerifier.create(data)
                .expectNext(10L)
                .verifyComplete();
    }

}