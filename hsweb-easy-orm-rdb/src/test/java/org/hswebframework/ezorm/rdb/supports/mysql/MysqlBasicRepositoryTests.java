package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicRepositoryTests;
import org.hswebframework.ezorm.rdb.supports.mssql.MSSQLConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerSchemaMetadata;

public class MysqlBasicRepositoryTests extends BasicRepositoryTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        return new MysqlSchemaMetadata("ezorm");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.MYSQL;
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {

        return new TestSyncSqlExecutor(new Mysql57ConnectionProvider());
    }
}
