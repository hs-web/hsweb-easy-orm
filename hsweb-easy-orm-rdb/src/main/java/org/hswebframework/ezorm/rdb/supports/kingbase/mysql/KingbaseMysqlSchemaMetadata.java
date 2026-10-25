package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.CompositeExceptionTranslation;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlBatchUpsertOperator;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlEnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlPaginator;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlDropIndexSqlBuilder;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;

/**
 * Aggregates the MySQL-mode SQL, metadata and exception capabilities for KingbaseES.
 */
public class KingbaseMysqlSchemaMetadata extends RDBSchemaMetadata {

    public KingbaseMysqlSchemaMetadata(String name) {
        super(name);

        addFeature(new KingbaseMysqlCreateTableSqlBuilder());
        addFeature(new KingbaseMysqlAlterTableSqlBuilder());
        addFeature(KingbaseMysqlCreateIndexSqlBuilder.INSTANCE);
        addFeature(PostgresqlDropIndexSqlBuilder.INSTANCE);
        addFeature(new MysqlPaginator());

        addFeature(new KingbaseMysqlIndexMetadataParser(this));
        addFeature(new KingbaseMysqlTableMetadataParser(this));
        addFeature(new KingbaseMysqlDialect());
        addFeature(new CompositeExceptionTranslation()
                       .add(FeatureUtils.r2dbcIsAlive(), () -> KingbaseMysqlR2DBCExceptionTranslation.of(this))
                       .add(KingbaseMysqlJDBCExceptionTranslation.of(this)));
    }

    @Override
    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata metadata = super.newTable(name);
        metadata.addFeature(new MysqlBatchUpsertOperator(metadata));
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
