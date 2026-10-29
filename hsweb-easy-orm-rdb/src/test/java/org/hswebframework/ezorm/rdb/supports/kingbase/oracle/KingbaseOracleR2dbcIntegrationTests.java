package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbase.AbstractKingbaseR2dbcIntegrationTests;
import org.hswebframework.ezorm.rdb.supports.kingbase.KingbaseTestContainer;
import org.junit.ClassRule;

public class KingbaseOracleR2dbcIntegrationTests extends AbstractKingbaseR2dbcIntegrationTests {

    @ClassRule
    public static final KingbaseTestContainer DATABASE = new KingbaseTestContainer("oracle");

    @Override
    protected String mode() {
        return "oracle";
    }

    @Override
    protected String defaultUrl() {
        return "r2dbc:kingbase://127.0.0.1:54322/test?mode=oracle";
    }

    @Override
    protected RDBSchemaMetadata createSchema(String schemaName) {
        return new KingbaseOracleSchemaMetadata(schemaName);
    }
}
