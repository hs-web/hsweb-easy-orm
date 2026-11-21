package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.AbstractBatchUpsertIntegrationTest;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseOfficialTestDatabase;
import org.junit.ClassRule;

/** Inherits upstream Oracle batch upsert test base at 96b61b0b without overriding tests or assertions. */
public class KingbaseOracleOfficialBatchUpsertTests extends AbstractBatchUpsertIntegrationTest {

    @ClassRule
    public static final KingbaseOfficialTestDatabase DATABASE = new KingbaseOfficialTestDatabase("oracle");

    @Override
    protected RDBSchemaMetadata getSchema() {
        return DATABASE.newSchema();
    }

    @Override
    protected Dialect getDialect() {
        return new KingbaseOracleDialect();
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return DATABASE.sync();
    }
}
