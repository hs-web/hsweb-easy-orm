package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicCommonTests;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;

public class MSSQLBasicTest extends BasicCommonTests {

    @Override
    protected RDBSchemaMetadata getSchema() {
        return new SqlServerSchemaMetadata("dbo");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.MSSQL;
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return new TestSyncSqlExecutor(new MSSQLConnectionProvider());
    }
}
