package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicCommonTests;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.posgres.PostgreSQLAlterTableSqlBuilder;

public class PostgreSQLBasicTest extends BasicCommonTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        RDBSchemaMetadata schema=new RDBSchemaMetadata();
        schema.setName("public");
        schema.addFeature(new PostgreSQLAlterTableSqlBuilder());
        return schema;
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
