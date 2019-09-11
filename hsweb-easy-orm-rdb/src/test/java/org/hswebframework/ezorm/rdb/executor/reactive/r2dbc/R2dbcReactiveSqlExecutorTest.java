package org.hswebframework.ezorm.rdb.executor.reactive.r2dbc;

import io.r2dbc.h2.H2ConnectionFactory;
import io.r2dbc.mssql.MssqlConnectionFactory;
import io.r2dbc.postgresql.PostgresqlConnectionFactory;
import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactoryOptions;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.supports.h2.H2R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.mssql.MSSQLR2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerR2dbcReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgreSQLR2dbcConnectionProvider;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.stream.Collectors;

import static io.r2dbc.spi.ConnectionFactoryOptions.*;
import static org.hswebframework.ezorm.rdb.executor.SqlRequests.of;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.lowerCase;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.map;

public class R2dbcReactiveSqlExecutorTest {

    @Test
    public void testH2Database() {

        executeTest(new TestReactiveSqlExecutor(new H2R2dbcConnectionProvider()));

    }

    @Test
    public void testPostgreSQL() {


        executeTest(new TestReactiveSqlExecutor(new PostgreSQLR2dbcConnectionProvider()));

    }

    @Test
    public void testMSSQL() {



        executeTest(new TestReactiveSqlExecutor("@arg",new MSSQLR2dbcConnectionProvider()));

    }


    public void executeTest(R2dbcReactiveSqlExecutor sqlExecutor) {

        try {
            Mono<Void> mono = sqlExecutor.execute(Mono.just(of("create table test_r2dbc(id varchar(32) primary key)")));

            StepVerifier.create(mono).verifyComplete();

            //插入100条数据
            Mono<Integer> update = sqlExecutor.update(Flux.range(1, 100).map(i -> of("insert into test_r2dbc(id)values(?)", "" + i)));

            StepVerifier.create(update)
                    .expectNext(100)
                    .verifyComplete();

            //查询id并合计
            Mono<Integer> sum = sqlExecutor.select(Mono.just(of("select id from test_r2dbc")), lowerCase(map()))
                    .map(map -> map.get("id"))
                    .map(String::valueOf)
                    .collect(Collectors.summingInt(Integer::valueOf));

            StepVerifier.create(sum)
                    .expectNext(5050)
                    .verifyComplete();
        } finally {
            sqlExecutor.execute(Mono.just(of("drop table test_r2dbc"))).block();
        }
    }
}