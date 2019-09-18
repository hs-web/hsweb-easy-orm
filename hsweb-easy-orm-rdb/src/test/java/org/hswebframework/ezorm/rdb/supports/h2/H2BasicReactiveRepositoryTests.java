package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicReactiveRepositoryTests;
import org.hswebframework.ezorm.rdb.supports.BasicRepositoryTests;

public class H2BasicReactiveRepositoryTests extends BasicReactiveRepositoryTests {
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
}
