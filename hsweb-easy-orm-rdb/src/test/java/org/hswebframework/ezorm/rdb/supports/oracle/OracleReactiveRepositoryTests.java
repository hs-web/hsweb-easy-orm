package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.TestJdbcReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicReactiveRepositoryTests;
import org.hswebframework.ezorm.rdb.supports.h2.H2R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;

public class OracleReactiveRepositoryTests extends BasicReactiveRepositoryTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        return new OracleSchemaMetadata("SYSTEM");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.ORACLE;
    }

    @Override
    protected ReactiveSqlExecutor getSqlExecutor() {

        return new TestJdbcReactiveSqlExecutor(new OracleConnectionProvider());
    }
}
