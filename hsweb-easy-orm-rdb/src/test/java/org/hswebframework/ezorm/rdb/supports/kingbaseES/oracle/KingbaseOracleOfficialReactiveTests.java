package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicReactiveTests;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseOfficialTestDatabase;
import org.junit.ClassRule;

/** Inherits upstream Oracle test base at 96b61b0b without overriding tests or assertions. */
public class KingbaseOracleOfficialReactiveTests extends BasicReactiveTests {

    @ClassRule
    public static final KingbaseOfficialTestDatabase DATABASE = new KingbaseOfficialTestDatabase("oracle");

    @Override
    protected RDBSchemaMetadata getSchema() {
        return DATABASE.newSchema();
    }

    @Override
    protected Dialect getDialect() {
        return new KingbaseOracleDialect();
    }

    @Override
    protected ReactiveSqlExecutor getSqlExecutor() {
        return DATABASE.reactive();
    }
}
