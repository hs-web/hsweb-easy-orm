package org.hswebframework.ezorm.rdb.supports.kingbaseES.mysql;

import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicReactiveTests;
import org.hswebframework.ezorm.rdb.supports.BasicTestEntity;
import org.junit.ClassRule;
import org.junit.Test;
import reactor.test.StepVerifier;

/** Inherits upstream 4.2 BasicReactiveTests at 96b61b0b without overriding any test. */
public class KingbaseMysqlOfficialReactiveTests extends BasicReactiveTests {

    @ClassRule
    public static final KingbaseMysqlOfficialTestDatabase DATABASE = new KingbaseMysqlOfficialTestDatabase();

    @Override
    protected RDBSchemaMetadata getSchema() {
        return DATABASE.newSchema();
    }

    @Override
    protected Dialect getDialect() {
        return new KingbaseMysqlDialect();
    }

    @Override
    protected ReactiveSqlExecutor getSqlExecutor() {
        return DATABASE.reactive();
    }

    // Copied unchanged from upstream MysqlReactiveTests.testExceptionTrans at 96b61b0b.
    @Test
    public void testExceptionTrans() {
        BasicTestEntity entity = new BasicTestEntity();
        entity.setId("1234");
        entity.setName("test");
        entity.setState((byte) 1);

        repository.insert(entity)
                  .then(repository.insert(entity))
                  .as(StepVerifier::create)
                  .verifyError(DuplicateKeyException.class);
    }
}
