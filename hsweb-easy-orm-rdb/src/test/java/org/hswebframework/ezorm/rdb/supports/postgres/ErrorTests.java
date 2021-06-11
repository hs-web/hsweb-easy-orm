package org.hswebframework.ezorm.rdb.supports.postgres;

import io.r2dbc.spi.R2dbcBadGrammarException;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.junit.Test;
import reactor.test.StepVerifier;

public class ErrorTests {


    @Test
    public void testReactiveError() {
        TestReactiveSqlExecutor sqlExecutor = new TestReactiveSqlExecutor(new PostgresqlR2dbcConnectionProvider());

        sqlExecutor.select("select 1 from test.table")
                   .as(StepVerifier::create)
                   .expectError(R2dbcBadGrammarException.class)
                   .verify();
    }



}
