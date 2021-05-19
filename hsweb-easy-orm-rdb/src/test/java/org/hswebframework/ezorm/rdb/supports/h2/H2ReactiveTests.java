package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicReactiveTests;
import org.hswebframework.ezorm.rdb.supports.BasicTestEntity;
import org.junit.Test;
import reactor.test.StepVerifier;

public class H2ReactiveTests extends BasicReactiveTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        return new H2SchemaMetadata("PUBLIC");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.H2;
    }

    @Override
    protected ReactiveSqlExecutor getSqlExecutor() {

        return new TestReactiveSqlExecutor(new H2R2dbcConnectionProvider());
    }

    @Test
    public void errorTest() {
        BasicTestEntity e = new BasicTestEntity();
        e.setId("id-123");
        e.setName("test");
        e.setState((byte) 1);
        repository
                .insert(e)
                .then(repository.insert(e))
                .as(StepVerifier::create)
                .expectError(DuplicateKeyException.class)
                .verify();
        ;

    }
}
