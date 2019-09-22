package org.hswebframework.ezorm.spring.executor;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.stream.Collectors;


@SpringBootTest(classes = ReactiveTestApplication.class)
@RunWith(SpringJUnit4ClassRunner.class)
public class TransactionR2dbcSqlExecutorTest {

    @Autowired
    private ReactiveSqlExecutor sqlExecutor;

    @Autowired
    private TxTest txTest;

    @Test
    public void testTransaction() {

        Mono.just("drop table if exists spring_r2dbc_tx_test")
                .map(SqlRequests::of)
                .as(sqlExecutor::execute)
                .block();
        Mono.just("create table spring_r2dbc_tx_test(id varchar(32) not null primary key,name varchar(32))")
                .map(SqlRequests::of)
                .as(sqlExecutor::execute)
                .as(StepVerifier::create)
                .expectFusion()
                .verifyComplete();
        //rollback
        txTest.test(true)
                .collect(Collectors.summingInt(Integer::intValue))
                .as(StepVerifier::create)
                .verifyError();

        Mono.just("select count(1) from spring_r2dbc_tx_test")
                .map(SqlRequests::of)
                .as(req -> sqlExecutor.select(req, ResultWrappers.column("count", Number.class::cast)))
                .map(Number::intValue)
                .as(StepVerifier::create)
                .expectNext(0)
                .verifyComplete();

        //commit
        txTest.test(false)
                .collect(Collectors.summingInt(Integer::intValue))
                .as(StepVerifier::create)
                .expectNext(2)
                .verifyComplete();

        Mono.just("select count(1) from spring_r2dbc_tx_test")
                .map(SqlRequests::of)
                .as(req -> sqlExecutor.select(req, ResultWrappers.column("count", Number.class::cast)))
                .map(Number::intValue)
                .as(StepVerifier::create)
                .expectNext(2)
                .verifyComplete();

    }


}