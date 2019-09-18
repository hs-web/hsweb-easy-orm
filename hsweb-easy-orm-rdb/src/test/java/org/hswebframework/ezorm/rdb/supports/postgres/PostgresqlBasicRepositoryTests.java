package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicRepositoryTests;
import org.hswebframework.ezorm.rdb.supports.mysql.Mysql57ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.posgres.PostgresqlSchemaMetadata;

public class PostgresqlBasicRepositoryTests extends BasicRepositoryTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        return new PostgresqlSchemaMetadata("public");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.POSTGRES;
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {

        return new TestSyncSqlExecutor(new PostgresqlConnectionProvider());
    }
}
