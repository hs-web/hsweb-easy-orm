package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.junit.ClassRule;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseOfficialTestDatabase;

import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.AbstractBatchUpsertIntegrationTest;

public class KingbaseSqlServerOfficialBatchUpsertTests extends AbstractBatchUpsertIntegrationTest {

    @ClassRule
    public static final KingbaseOfficialTestDatabase DATABASE = new KingbaseOfficialTestDatabase("sqlserver");

    @Override
    protected RDBSchemaMetadata getSchema() {
        return DATABASE.newSchema();
    }

    @Override
    protected Dialect getDialect() {
        return new KingbaseSqlServerDialect();
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return DATABASE.sync();
    }
}
