package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.core.meta.ColumnMetadata;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.codec.BooleanValueCodec;
import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.CompositeExceptionTranslation;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.NullTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleBatchUpsertOperator;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleEnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleInsertSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleJsonTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.oracle.OraclePaginator;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlDropIndexSqlBuilder;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;

/**
 * Schema metadata and DDL features for KingbaseES Oracle compatibility mode.
 */
public class KingbaseOracleSchemaMetadata extends RDBSchemaMetadata {

    public KingbaseOracleSchemaMetadata(String name) {
        super(name);
        addFeature(new KingbaseOracleCreateTableSqlBuilder());
        addFeature(new KingbaseOracleAlterTableSqlBuilder());
        addFeature(KingbaseOracleCreateIndexSqlBuilder.INSTANCE);
        addFeature(PostgresqlDropIndexSqlBuilder.INSTANCE);
        addFeature(new OraclePaginator());
        addFeature(new KingbaseOracleTableMetadataParser(this));
        addFeature(KingbaseOracleIndexMetadataParser.of(this));
        addFeature(new KingbaseOracleDialect());
        // Oracle mode treats the empty string as NULL; comparisons with '' cannot filter paths correctly.
        addFeature(new NullTermFragmentBuilder("empty", "为空字符", false));
        addFeature(new NullTermFragmentBuilder("notempty", "不为空字符", true));
        addFeature(new NullTermFragmentBuilder(TermType.nempty, "不为空字符", true));
        addFeature(new CompositeExceptionTranslation()
            .add(FeatureUtils.r2dbcIsAlive(), () -> KingbaseOracleR2DBCExceptionTranslation.of(this))
            .add(KingbaseOracleJDBCExceptionTranslation.of(this)));
    }

    @Override
    public void addTable(RDBTableMetadata metadata) {
        metadata.addFeature(OracleInsertSqlBuilder.of(metadata));
        super.addTable(metadata);
    }

    @Override
    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata metadata = super.newTable(name);
        metadata.addFeature(OracleInsertSqlBuilder.of(metadata));
        metadata.addFeature(new OracleBatchUpsertOperator(metadata));
        metadata.setOnColumnAdded(column -> {
            if (column.getValueCodec() != null
                && column.getValueCodec().getClass() == BooleanValueCodec.class
                && column.getType() != null
                && column.getType().sqlTypeIsNumber()) {
                column.setValueCodec(new BooleanValueCodec(column.getSqlType()) {
                    @Override
                    public Object encodeNull(ColumnMetadata metadata) {
                        // NUMBER-backed booleans must bind NULL with the same numeric type as 1/0.
                        return NullValue.of(Integer.class, ((RDBColumnMetadata) metadata).getType());
                    }
                });
            }
            if (column.getValueCodec() instanceof EnumValueCodec
                && ((EnumValueCodec) column.getValueCodec()).isToMask()) {
                column.addFeature(OracleEnumInFragmentBuilder.in);
                column.addFeature(OracleEnumInFragmentBuilder.notIn);
            }
            OracleJsonTermFragmentBuilder.addJsonFeatures(column);
        });
        return metadata;
    }
}
