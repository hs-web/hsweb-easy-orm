package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicReactiveRepositoryTests;
import org.hswebframework.ezorm.rdb.supports.h2.H2R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSchemaMetadata;

public class MSSQLBasicReactiveRepositoryTests extends BasicReactiveRepositoryTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        return new SqlServerSchemaMetadata("dbo");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.MSSQL;
    }

    @Override
    protected ReactiveSqlExecutor getSqlExecutor() {

        return new TestReactiveSqlExecutor("@arg",new MSSQLR2dbcConnectionProvider());
    }
}
