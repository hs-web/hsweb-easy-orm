package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicCommonTests;
import org.hswebframework.ezorm.rdb.supports.mssql.MSSQLR2dbcConnectionProvider;

public class H2BasicTest extends BasicCommonTests {

    @Override
    protected RDBSchemaMetadata getSchema() {
        return new H2SchemaMetadata("PUBLIC");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.H2;
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return new TestSyncSqlExecutor(new H2ConnectionProvider());
    }
}
