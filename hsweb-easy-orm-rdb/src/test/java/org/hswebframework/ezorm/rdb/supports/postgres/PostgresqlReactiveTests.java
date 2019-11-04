package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicReactiveTests;
import org.hswebframework.ezorm.rdb.supports.BasicTestEntity;
import org.junit.Ignore;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Date;
import java.util.List;
import java.util.Map;

public class PostgresqlReactiveTests extends BasicReactiveTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        return new PostgresqlSchemaMetadata("public");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.POSTGRES;
    }

    @Override
    protected ReactiveSqlExecutor getSqlExecutor() {

        return new TestReactiveSqlExecutor(new PostgresqlR2dbcConnectionProvider());
    }

    @Test
    public void testException(){
        repository.insert(Mono.just(BasicTestEntity.builder()
                .name("test")
                .id("test")
                .state((byte)1)
                .build()))
                .as(StepVerifier::create)
                .expectNext(1)
                .verifyComplete();

        repository.insert(Mono.just(BasicTestEntity.builder()
                .name("test")
                .id("test")
                .state((byte)1)
                .build()))
                .as(StepVerifier::create)
                .expectError(DuplicateKeyException.class)
                .verify();
    }

    @Test
    @Ignore
    public void benchmark() {
        long time = System.currentTimeMillis();

        StepVerifier
                .create(repository.insertBatch(Flux.range(0, 100000)
                        .map(integer -> BasicTestEntity.builder()
                                .id("id:" + integer)
                                .balance(1000L)
                                .name("test:" + integer)
                                .createTime(new Date())
                                .state((byte) 1)
                                .build())
                        .buffer(1000)))
                .expectNext(100000)
                .verifyComplete();
        System.out.println(System.currentTimeMillis() - time);
    }
}
