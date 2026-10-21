package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbase.AbstractKingbaseR2dbcIntegrationTests;

public class KingbaseOracleR2dbcIntegrationTests extends AbstractKingbaseR2dbcIntegrationTests {

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
