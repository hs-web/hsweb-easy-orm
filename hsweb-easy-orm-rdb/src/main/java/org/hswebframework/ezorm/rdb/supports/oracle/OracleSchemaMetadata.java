package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

public class OracleSchemaMetadata extends RDBSchemaMetadata {

    public OracleSchemaMetadata(String name) {
        super(name);
        addFeature(new OraclePaginator());
    }

    @Override
    public void addTable(RDBTableMetadata metadata) {
        metadata.addFeature(OracleInsertSqlBuilder.of(metadata));
        super.addTable(metadata);
    }
}
