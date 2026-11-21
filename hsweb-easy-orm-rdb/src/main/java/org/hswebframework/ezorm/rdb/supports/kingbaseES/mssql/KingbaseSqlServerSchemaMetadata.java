package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.core.meta.ColumnMetadata;
import org.hswebframework.ezorm.rdb.codec.DateTimeCodec;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.CompositeExceptionTranslation;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerSchemaMetadata;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;

import java.sql.JDBCType;
import java.sql.Timestamp;
import java.util.function.Consumer;

/** Inherits SQL Server DDL, queries, paging, upsert and column features. */
public class KingbaseSqlServerSchemaMetadata extends SqlServerSchemaMetadata {

    public KingbaseSqlServerSchemaMetadata(String name) {
        super(name);
        addFeature(new KingbaseSqlServerCreateTableSqlBuilder());
        addFeature(new KingbaseSqlServerAlterTableSqlBuilder());
        addFeature(new KingbaseSqlServerDialect());
        addFeature(new KingbaseSqlServerTableMetadataParser(this));
        addFeature(new KingbaseSqlServerIndexMetadataParser(this));
        addFeature(new CompositeExceptionTranslation()
            .add(FeatureUtils.r2dbcIsAlive(), () -> KingbaseSqlServerR2DBCExceptionTranslation.of(this))
            .add(KingbaseSqlServerJDBCExceptionTranslation.of(this)));
    }

    @Override
    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata table = super.newTable(name);
        Consumer<RDBColumnMetadata> nativeListener = table.getOnColumnAdded();
        table.setOnColumnAdded(column -> {
            nativeListener.accept(column);
            KingbaseSqlServerJsonTermFragmentBuilder.addJsonFeatures(column);
            if (column.getValueCodec() instanceof DateTimeCodec && column.getSqlType() == JDBCType.TIMESTAMP) {
                DateTimeCodec delegate = (DateTimeCodec) column.getValueCodec();
                column.setValueCodec(new ValueCodec<Object, Object>() {
                    @Override
                    public Object encode(Object value) {
                        return delegate.encode(value);
                    }

                    @Override
                    public Object encodeNull(ColumnMetadata metadata) {
                        return delegate.encodeNull(metadata);
                    }

                    @Override
                    public Object decode(Object value) {
                        if (value instanceof String) {
                            try {
                                value = Timestamp.valueOf((String) value);
                            } catch (IllegalArgumentException ignore) {
                                // Other input formats retain the original codec's behavior.
                            }
                        }
                        return delegate.decode(value);
                    }
                });
            }
        });
        return table;
    }
}
