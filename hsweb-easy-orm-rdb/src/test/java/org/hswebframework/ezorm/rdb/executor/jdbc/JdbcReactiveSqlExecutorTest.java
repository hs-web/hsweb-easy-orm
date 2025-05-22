package org.hswebframework.ezorm.rdb.executor.jdbc;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.TestJdbcReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
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
        sqlExecutor = new TestJdbcReactiveSqlExecutor(new H2ConnectionProvider());
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
                .doOnError(Throwable::printStackTrace)
                .doFinally(s-> System.out.println(Thread.currentThread()));

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

        Mono<Long> count = sqlExecutor.select(Flux.range(0, 10)
                .map(String::valueOf)
                .map(num -> of("select * from test where id = ?", num)), map())
                .doOnError(Throwable::printStackTrace)
                .map(map -> map.get("ID"))
                .count();

        StepVerifier.create(count)
                .expectNext(10L)
                .verifyComplete();
    }

}