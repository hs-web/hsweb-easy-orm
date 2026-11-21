package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.junit.ClassRule;
import org.hswebframework.ezorm.rdb.supports.json.AbstractJsonQueryIntegrationTest;
import org.hswebframework.ezorm.rdb.supports.json.JsonType;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseOfficialTestDatabase;

import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;

public class KingbaseSqlServerOfficialJsonTests extends AbstractJsonQueryIntegrationTest {

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

    @Override
    protected DataType getJsonType() {
        return JsonType.NVARCHAR_MAX;
    }
}
