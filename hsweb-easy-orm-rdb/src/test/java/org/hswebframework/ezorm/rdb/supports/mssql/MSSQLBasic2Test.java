package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicCommonTests;

public class MSSQLBasic2Test extends BasicCommonTests {

    @Override
    protected RDBSchemaMetadata getSchema() {
        SqlServerSchemaMetadata schemaMetadata= new SqlServerSchemaMetadata("dbo");
        schemaMetadata.addFeature(new SqlServerPaginator());
        return schemaMetadata;
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
