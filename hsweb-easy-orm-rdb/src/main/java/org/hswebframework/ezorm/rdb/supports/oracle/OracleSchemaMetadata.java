package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;

public class OracleSchemaMetadata extends RDBSchemaMetadata {

    public OracleSchemaMetadata(String name) {
        super(name);
        addFeature(new OraclePaginator());
    }
}
