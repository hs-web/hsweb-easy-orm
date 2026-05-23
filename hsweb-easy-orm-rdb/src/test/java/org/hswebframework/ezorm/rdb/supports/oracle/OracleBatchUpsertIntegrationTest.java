package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.AbstractBatchUpsertIntegrationTest;

public class OracleBatchUpsertIntegrationTest extends AbstractBatchUpsertIntegrationTest {

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
