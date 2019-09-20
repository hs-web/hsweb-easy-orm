package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicCommonTests;
import org.hswebframework.ezorm.rdb.supports.posgres.PostgresqlSchemaMetadata;

public class PostgresqlBasicTest extends BasicCommonTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        return new PostgresqlSchemaMetadata("public");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.POSTGRES;
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return new TestSyncSqlExecutor(new PostgresqlConnectionProvider());
    }
}
