package org.hswebframework.ezorm.rdb.supports.clickhouse;

import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.CompositeExceptionTranslation;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.update.DefaultUpdateSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.clickhouse.sqlBuilder.ClickhouseDeleteSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.clickhouse.sqlBuilder.ClickhouseUpdateSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.*;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;

/**
 * @className ClickhouseSchemaMetadata
 * @Description TODO
 * @Author dengpengyu
 * @Date 2023/9/5 9:36
 * @Vesion 1.0
 */
public class ClickhouseSchemaMetadata extends RDBSchemaMetadata {
    public ClickhouseSchemaMetadata(String name) {
        super(name);
        addFeature(new MysqlPaginator());
        // TODO 后续增加建表
        //读取表元数据
        addFeature(new ClickhouseTableMetadataParser(this));
        addFeature(new ClickhouseDialect());
        addFeature(new CompositeExceptionTranslation()
                .add(FeatureUtils.r2dbcIsAlive(), () -> MysqlR2DBCExceptionTranslation.of(this))
                .add(MysqlJDBCExceptionTranslation.of(this))
        );
    }

    @Override
    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata metadata = super.newTable(name);
        metadata.addFeature(ClickhouseUpdateSqlBuilder.of(metadata));
        metadata.addFeature(ClickhouseDeleteSqlBuilder.of(metadata));
        metadata.setOnColumnAdded(column -> {
            if (column.getValueCodec() instanceof EnumValueCodec && ((EnumValueCodec) column.getValueCodec()).isToMask()) {
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
