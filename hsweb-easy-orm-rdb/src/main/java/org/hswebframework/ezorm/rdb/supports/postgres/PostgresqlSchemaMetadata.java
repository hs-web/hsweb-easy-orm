package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatures;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.CompositeExceptionTranslation;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;

public class PostgresqlSchemaMetadata extends RDBSchemaMetadata {

    public PostgresqlSchemaMetadata(String name) {
        super(name);
        addFeature(new PostgresqlPaginator());
        addFeature(PostgresqlDropIndexSqlBuilder.INSTANCE);
        addFeature(new PostgresqlAlterTableSqlBuilder());

        addFeature(new PostgresqlTableMetadataParser(this));
        addFeature(new PostgresqlIndexMetadataParser(this));
        addFeature(Dialect.POSTGRES);

        addFeature(new CompositeExceptionTranslation()
                .add(FeatureUtils.r2dbcIsAlive(), () -> PostgresqlR2DBCExceptionTranslation.of(this))
        );
    }

    @Override
    public void addTable(RDBTableMetadata metadata) {
        metadata.addFeature(new PostgresqlBatchUpsertOperator(metadata));
        super.addTable(metadata);
    }

    @Override
    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata metadata = super.newTable(name);
        metadata.addFeature(new PostgresqlBatchUpsertOperator(metadata));
        metadata.setOnColumnAdded(column->{
            if(column.getValueCodec() instanceof EnumValueCodec &&((EnumValueCodec) column.getValueCodec()).isToMask()){
                column.addFeature(PostgresqlEnumInFragmentBuilder.in);
                column.addFeature(PostgresqlEnumInFragmentBuilder.notIn);
            }
        });
        return metadata;
    }
}
