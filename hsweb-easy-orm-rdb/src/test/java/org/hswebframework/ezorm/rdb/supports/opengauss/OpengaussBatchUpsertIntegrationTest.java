package org.hswebframework.ezorm.rdb.supports.opengauss;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.AbstractBatchUpsertIntegrationTest;

public class OpengaussBatchUpsertIntegrationTest extends AbstractBatchUpsertIntegrationTest {

    @Override
    protected RDBSchemaMetadata getSchema() {
        return new OpengaussSchemaMetadata("gaussdb");
    }

    @Override
    protected Dialect getDialect() {
        return new OpengaussDialect();
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return new TestSyncSqlExecutor(new OpengaussConnectionProvider());
    }
}
