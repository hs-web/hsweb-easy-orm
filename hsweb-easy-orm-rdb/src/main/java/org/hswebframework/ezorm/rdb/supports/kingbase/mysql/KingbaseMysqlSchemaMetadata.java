package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.codec.DateTimeCodec;
import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.CompositeExceptionTranslation;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlBatchUpsertOperator;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlEnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlPaginator;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlDropIndexSqlBuilder;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;

import java.sql.Timestamp;
import java.util.Date;

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
            if (column.getValueCodec() instanceof DateTimeCodec
                && column.getJavaType() != null && Date.class.isAssignableFrom(column.getJavaType())) {
                ValueCodec<?, ?> delegate = column.getValueCodec();
                column.setValueCodec(new ValueCodec<Object, Object>() {
                    @Override
                    public Object encode(Object value) {
                        return delegate.encode(value);
                    }

                    @Override
                    public Object decode(Object value) {
                        // KES 驱动可能将 datetime(6) 返回为带小数秒的字符串。
                        if (value instanceof String) {
                            try {
                                value = Timestamp.valueOf((String) value);
                            } catch (IllegalArgumentException ignore) {
                                // 非 SQL 时间戳字符串继续由原编解码器按原格式解析。
                            }
                        }
                        return delegate.decode(value);
                    }
                });
            }
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
