package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatures;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.SelectColumnFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;

import java.util.Optional;

public class PostgresqlSelectColumnFragmentBuilder extends SelectColumnFragmentBuilder {

    private final TableOrViewMetadata metadata;

    private PostgresqlSelectColumnFragmentBuilder(TableOrViewMetadata metadata) {
        super(metadata);
        this.metadata = metadata;
    }

    public static PostgresqlSelectColumnFragmentBuilder of(TableOrViewMetadata metadata) {
        return new PostgresqlSelectColumnFragmentBuilder(metadata);
    }

    @Override
    public SqlFragments createFragments(QueryOperatorParameter parameter, SelectColumn column) {
        if (!"count".equals(column.getFunction())
            || column.getColumn() == null
            || column.getColumn().contains(".")
            || (column.getOpts() != null && !column.getOpts().isEmpty())
            || !parameter.getJoins().isEmpty()
            || !metadata.getForeignKeys().isEmpty()
            || metadata.findFeature(FunctionFragmentBuilder.createFeatureId("count")).orElse(null) != RDBFeatures.count) {
            return super.createFragments(parameter, column);
        }

        Optional<RDBColumnMetadata> columnMetadata = metadata.findColumn(column.getColumn());
        if (columnMetadata.isEmpty() || !columnMetadata.get().isNotNull()) {
            return super.createFragments(parameter, column);
        }

        // JOIN 可能把表中非空的列扩展为 NULL；只有无 JOIN 的普通计数才等价于行数计数。
        String alias = column.getAlias();
        if (alias == null) {
            alias = columnMetadata.get().getAlias();
            if (alias.contains(".")) {
                return SqlFragments.of("count(*)", "as", alias);
            }
        }
        return SqlFragments.of("count(*)", "as", metadata.getDialect().quote(alias, false));
    }
}
