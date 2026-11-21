package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CommonCreateIndexSqlBuilder;

/** Reuses the same index generation as native Oracle. */
public class KingbaseOracleCreateIndexSqlBuilder extends CommonCreateIndexSqlBuilder {

    public static final KingbaseOracleCreateIndexSqlBuilder INSTANCE = new KingbaseOracleCreateIndexSqlBuilder();
}
