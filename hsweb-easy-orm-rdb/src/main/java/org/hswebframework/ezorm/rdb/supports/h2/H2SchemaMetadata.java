package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.CompositeExceptionTranslation;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.BatchInsertSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlR2DBCExceptionTranslation;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;

public class H2SchemaMetadata extends RDBSchemaMetadata {

    public H2SchemaMetadata(String name) {
        super(name);
        addFeature(new H2CreateTableSqlBuilder());
        addFeature(new H2AlterTableSqlBuilder());
        addFeature(new H2Paginator());
        addFeature(new H2IndexMetadataParser(this));
        addFeature(new H2TableMetadataParser(this));
        addFeature(Dialect.H2);
        addFeature(new CompositeExceptionTranslation()
                           .add(FeatureUtils.r2dbcIsAlive(), () -> H2R2DBCExceptionTranslation.of(this))
                           .add(H2JDBCExceptionTranslation.of(this))
        );
    }

    @Override
    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata metadata = super.newTable(name);
        metadata.addFeature(BatchInsertSqlBuilder.of(metadata));
        metadata.setOnColumnAdded(column -> {
            if (column.getValueCodec() instanceof EnumValueCodec && ((EnumValueCodec) column.getValueCodec()).isToMask()) {
                column.addFeature(H2EnumInFragmentBuilder.in);
                column.addFeature(H2EnumInFragmentBuilder.notIn);
            }
        });
        return metadata;
    }

    @Override
    public void addTable(RDBTableMetadata metadata) {
        metadata.addFeature(BatchInsertSqlBuilder.of(metadata));
        super.addTable(metadata);
    }
}
