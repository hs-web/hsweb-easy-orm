package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.rdb.supports.oracle.OracleDialect;

/**
 * KingbaseES dialect identity using the base Oracle type mappings and SQL rendering.
 */
public class KingbaseOracleDialect extends OracleDialect {

    public KingbaseOracleDialect() {
        registerDataType("varchar", convertDataType("varchar2"));
        registerDataType("pg_catalog.varchar", convertDataType("varchar2"));
        registerDataType("numeric", convertDataType("number"));
        registerDataType("timestamp without time zone", convertDataType("timestamp"));
        registerDataType("text", convertDataType("clob"));
        registerDataType("bytea", convertDataType("blob"));
        registerDataType("double precision", convertDataType("double"));
    }

    @Override
    public String getId() {
        return "kingbase-oracle";
    }

    @Override
    public String getName() {
        return "KingbaseOracle";
    }
}
