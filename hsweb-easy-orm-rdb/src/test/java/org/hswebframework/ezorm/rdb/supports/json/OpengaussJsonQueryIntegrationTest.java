package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.opengauss.OpengaussConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.opengauss.OpengaussDialect;
import org.hswebframework.ezorm.rdb.supports.opengauss.OpengaussSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.postgres.JsonbType;

public class OpengaussJsonQueryIntegrationTest extends AbstractJsonQueryIntegrationTest {

    @Override
    protected RDBSchemaMetadata getSchema() {
        return new OpengaussSchemaMetadata("gaussdb");
    }

    @Override
    protected Dialect getDialect() {
        return new OpengaussDialect();
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return new TestSyncSqlExecutor(new OpengaussConnectionProvider());
    }

    @Override
    protected DataType getJsonType() {
        return JsonbType.INSTANCE;
    }

    @Override
    protected boolean supportContained() {
        return true;
    }
}
