package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicCommonTests;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSchemaMetadata;

public class OracleBasicTest extends BasicCommonTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        RDBSchemaMetadata schema=new RDBSchemaMetadata();
        schema.setName("SYSTEM");

        return schema;
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.ORACLE;
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return new TestSyncSqlExecutor(new OracleConnectionProvider());
    }
}
