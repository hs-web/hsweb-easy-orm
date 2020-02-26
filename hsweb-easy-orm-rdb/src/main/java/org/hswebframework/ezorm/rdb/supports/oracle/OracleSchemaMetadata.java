package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;

public class OracleSchemaMetadata extends RDBSchemaMetadata {

    public OracleSchemaMetadata(String name) {
        super(name);
        addFeature(new OraclePaginator());

        addFeature(new OracleTableMetadataParser(this));

        addFeature(Dialect.ORACLE);
    }


    @Override
    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata metadata = super.newTable(name);
        metadata.addFeature(OracleInsertSqlBuilder.of(metadata));
        return metadata;
    }
}
