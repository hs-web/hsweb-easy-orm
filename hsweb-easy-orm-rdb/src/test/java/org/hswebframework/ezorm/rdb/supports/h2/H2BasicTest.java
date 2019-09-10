package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicCommonTests;

public class H2BasicTest extends BasicCommonTests {

    @Override
    protected RDBSchemaMetadata getSchema() {
        RDBSchemaMetadata schema=new RDBSchemaMetadata();
        schema.setName("PUBLIC");
        return schema;
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.H2;
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return new TestSyncSqlExecutor(new H2ConnectionProvider());
    }
}
