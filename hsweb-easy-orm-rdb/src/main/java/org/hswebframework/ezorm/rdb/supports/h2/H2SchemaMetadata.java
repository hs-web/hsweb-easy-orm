package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.BatchInsertSqlBuilder;

public class H2SchemaMetadata extends RDBSchemaMetadata {

    public H2SchemaMetadata(String name) {
        super(name);
        addFeature(new H2CreateTableSqlBuilder());
        addFeature(new H2AlterTableSqlBuilder());
        addFeature(new H2Paginator());
        addFeature(new H2IndexMetadataParser(this));
        addFeature(new H2TableMetadataParser(this));
        addFeature(Dialect.H2);
    }

    @Override
    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata metadata = super.newTable(name);
        metadata.addFeature(BatchInsertSqlBuilder.of(metadata));
        return metadata;
    }
}
