package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.CompositeExceptionTranslation;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;

public class MysqlSchemaMetadata extends RDBSchemaMetadata {

    public MysqlSchemaMetadata(String name) {
        super(name);

        addFeature(new MysqlCreateTableSqlBuilder());
        addFeature(new MysqlAlterTableSqlBuilder());
        addFeature(new MysqlPaginator());

        addFeature(new MysqlIndexMetadataParser(this));
        addFeature(new MysqlTableMetadataParser(this));
        addFeature(Dialect.MYSQL);
        addFeature(new CompositeExceptionTranslation()
                           .add(FeatureUtils.r2dbcIsAlive(), () -> MysqlR2DBCExceptionTranslation.of(this))
        );
    }

    @Override
    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata metadata= super.newTable(name);
        metadata.addFeature(new MysqlBatchUpsertOperator(metadata));
        metadata.setOnColumnAdded(column->{
            if(column.getValueCodec() instanceof EnumValueCodec &&((EnumValueCodec) column.getValueCodec()).isToMask()){
                column.addFeature(MysqlEnumInFragmentBuilder.in);
                column.addFeature(MysqlEnumInFragmentBuilder.notIn);
            }
        });
        return metadata;
    }

    @Override
    public void addTable(RDBTableMetadata metadata) {
        metadata.addFeature(new MysqlBatchUpsertOperator(metadata));
        super.addTable(metadata);
    }
}
