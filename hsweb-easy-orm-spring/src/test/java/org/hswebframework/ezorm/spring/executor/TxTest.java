package org.hswebframework.ezorm.spring.executor;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

@Component
public class TxTest {

    @Autowired
    private ReactiveSqlExecutor sqlExecutor;

    @Transactional
    public Flux<Integer> test(boolean error) {
        return Flux.concat(
                Mono.just("insert into spring_r2dbc_tx_test values(?,?)")
                        .map(sql -> SqlRequests.of(sql, "test", "test"))
                        .as(sqlExecutor::update),

                Mono.just("insert into spring_r2dbc_tx_test values(?,?)")
                        .map(sql -> SqlRequests.of(sql, "test2", "test2"))
                        .as(sqlExecutor::update)
                        .flatMap(t -> error ? Mono.error(new RuntimeException()) : Mono.just(t))
        );

    }
}