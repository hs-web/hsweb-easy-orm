package org.hswebframework.ezorm.rdb.supports.kingbaseES.mysql;

import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicCommonTests;
import org.junit.ClassRule;

/** Inherits upstream 4.2 BasicCommonTests at 96b61b0b without overriding any test. */
public class KingbaseMysqlOfficialSyncTests extends BasicCommonTests {

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
    protected SyncSqlExecutor getSqlExecutor() {
        return DATABASE.sync();
    }
}
