package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicReactiveTests;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseOfficialTestDatabase;
import org.junit.ClassRule;

/** Inherits the native SQL Server common test base without overriding tests or assertions. */
public class KingbaseSqlServerOfficialReactiveTests extends BasicReactiveTests {

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
    protected ReactiveSqlExecutor getSqlExecutor() {
        return DATABASE.reactive();
    }
}
