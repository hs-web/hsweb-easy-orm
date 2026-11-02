package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.mssql.MSSQLConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerSchemaMetadata;

public class SqlServerJsonQueryIntegrationTest extends AbstractJsonQueryIntegrationTest {

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

    @Override
    protected DataType getJsonType() {
        return JsonType.NVARCHAR_MAX;
    }
}
