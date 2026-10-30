package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import org.hswebframework.ezorm.rdb.supports.oracle.OracleDialect;

/**
 * KingbaseES dialect identity using the base Oracle type mappings and SQL rendering.
 */
public class KingbaseOracleDialect extends OracleDialect {

    @Override
    public String getId() {
        return "kingbase-oracle";
    }

    @Override
    public String getName() {
        return "KingbaseOracle";
    }
}
