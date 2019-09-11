package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;

public class H2SchemaMetadata extends RDBSchemaMetadata {

    public H2SchemaMetadata(String name){
        super(name);
        addFeature(new H2CreateTableSqlBuilder());
        addFeature(new H2AlterTableSqlBuilder());
        addFeature(new H2Paginator());
        addFeature(H2IndexMetadataParser.of(this));
    }
}
