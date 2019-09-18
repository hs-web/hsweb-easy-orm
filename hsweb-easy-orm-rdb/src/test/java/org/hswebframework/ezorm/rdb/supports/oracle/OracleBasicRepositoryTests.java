package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicRepositoryTests;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;

public class OracleBasicRepositoryTests extends BasicRepositoryTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        return new OracleSchemaMetadata("SYSTEM");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.ORACLE;
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {

        return new TestSyncSqlExecutor(new OracleConnectionProvider());
    }
}
