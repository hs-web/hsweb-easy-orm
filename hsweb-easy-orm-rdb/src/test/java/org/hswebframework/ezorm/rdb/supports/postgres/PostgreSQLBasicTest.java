package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicCommonTests;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.posgres.PostgreSQLAlterTableSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.posgres.PostgreSQLSchemaMetadata;

public class PostgreSQLBasicTest extends BasicCommonTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        return new PostgreSQLSchemaMetadata("public");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.POSTGRES;
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return new TestSyncSqlExecutor(new PostgreSQLConnectionProvider());
    }
}
