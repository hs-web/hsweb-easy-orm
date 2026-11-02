package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.SimpleFunctionFragmentBuilder;

public class SqlServerSchemaMetadata extends RDBSchemaMetadata {

    public SqlServerSchemaMetadata(String name) {
        super(name);
        addFeature(new SqlServerCreateTableSqlBuilder());
        addFeature(new SqlServerAlterTableSqlBuilder());
        addFeature(new SqlServer2012Paginator());
        addFeature(new SqlServer2012TableMetadataParser(this));
        addFeature(new SqlServerIndexMetadataParser(this));
        addFeature(Dialect.MSSQL);
        addFeature(new SqlServerQuerySqlBuilder(this));
        addFeature(new SimpleFunctionFragmentBuilder("stddev_pop", "stdevp", "标准差(总体)"));
        addFeature(new SimpleFunctionFragmentBuilder("stddev_samp", "stdev", "标准差(样本)"));


    }

    @Override
    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata metadata = super.newTable(name);
        metadata.addFeature(new SqlServerBatchUpsertOperator(metadata));
        metadata.setOnColumnAdded(column -> {
            if (column.getValueCodec() instanceof EnumValueCodec && ((EnumValueCodec) column.getValueCodec()).isToMask()) {
                column.addFeature(SqlServerEnumInFragmentBuilder.in);
                column.addFeature(SqlServerEnumInFragmentBuilder.notIn);
            }
            SqlServerJsonTermFragmentBuilder.addJsonFeatures(column);
        });
        return metadata;
    }
}
