package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.metadata.DefaultValueCodecFactory;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.ValueCodecFactory;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.CompositeExceptionTranslation;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NotFillOrNullFragmentBuilder;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;

import java.util.Optional;

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

        ValueByTimeFunctionFragmentBuilder last = new ValueByTimeFunctionFragmentBuilder("last", "最后一值");
        ValueByTimeFunctionFragmentBuilder first = new ValueByTimeFunctionFragmentBuilder("first", "第一个值");

        addFeature(last);
        addFeature(first);

        addFeature((ValueCodecFactory) column -> {
            if(column.getType() instanceof ValueCodec){
                return Optional.of(
                    ((ValueCodec<?,?>) column.getType())
                );
            };
            return DefaultValueCodecFactory.COMMONS.createValueCodec(column);
        });
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
            if (column.getType() instanceof PostgresqlArrayType) {
                column.addFeature(PostgresqlArrayTermFragmentBuilder.in);
                column.addFeature(new NotFillOrNullFragmentBuilder(PostgresqlArrayTermFragmentBuilder.notIn));
                column.addFeature(PostgresqlArrayTermFragmentBuilder.contains);
                column.addFeature(new NotFillOrNullFragmentBuilder(PostgresqlArrayTermFragmentBuilder.notContains));
                column.addFeature(PostgresqlArrayTermFragmentBuilder.contained);
                column.addFeature(new NotFillOrNullFragmentBuilder(PostgresqlArrayTermFragmentBuilder.notContained));
                column.addFeature(PostgresqlArrayTermFragmentBuilder.overlap);
                column.addFeature(new NotFillOrNullFragmentBuilder(PostgresqlArrayTermFragmentBuilder.notOverlap));
            }
            if (column.getValueCodec() instanceof VectorType) {
                column.addFeature(new PostgresqlVectorDistanceFunctionFragmentBuilder());
                PostgresqlVectorDistanceTermFragmentBuilder.ALL.values().forEach(column::addFeature);
            }
            if (column.getType() instanceof JsonbType) {
                column.addFeature(PostgresqlJsonbExistTermFragmentBuilder.exist);
            }
        });
        return metadata;
    }
}
