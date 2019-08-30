package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBFeatureType;
import org.hswebframework.ezorm.rdb.meta.RDBFutures;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;
import org.hswebframework.ezorm.rdb.operator.dml.SelectColumn;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

@AllArgsConstructor(staticName = "of")
public class SelectColumnFragmentBuilder implements QuerySqlFragmentBuilder {

    TableOrViewMetadata metadata;

    @Override
    public SqlFragments createFragments(ComplexQueryParameter parameter) {

        List<SelectColumn> columns = parameter.getSelect();

        if (columns == null || columns.isEmpty()) {
            return PrepareSqlFragments.of().addSql("*");
        }

        PrepareSqlFragments fragments = columns.stream()
                .map(column -> this.createFragments(parameter, column))
                .filter(Objects::nonNull)
                .reduce(PrepareSqlFragments.of(), (main, source) -> main.addFragments(source).addSql(","));

        fragments.removeLastSql();

        return fragments;
    }

    private String getAlias(String owner, RDBColumnMetadata metadata, SelectColumn column) {

        if (column.getAlias() != null) {
            return metadata.getDialect().quote(column.getAlias());
        }
        String alias = metadata.getAlias();

        if (alias.contains(".")) {
            return alias;
        }
        if (owner != null) {
            return owner.concat(".").concat(metadata.getDialect().quote(alias));
        }
        return metadata.getDialect().quote(alias);

    }

    public PrepareSqlFragments createFragments(ComplexQueryParameter parameter, SelectColumn column) {
        if (column instanceof NativeSqlFragments) {
            return PrepareSqlFragments.of()
                    .addSql(((NativeSqlFragments) column).getSql())
                    .addParameter(((NativeSqlFragments) column).getParameters());
        }
        String columnStr = column.getColumn();
        //关联表 table.column
        if (columnStr.contains(".")) {
            String[] arr = columnStr.split("[.]");
            return parameter.findJoin(arr[0])
                    .flatMap(join -> metadata.getSchema()
                            .getTableOrView(join.getTarget())
                            .flatMap(table -> table.findColumn(arr[1]))
                            .flatMap(columnMetadata ->
                                    createFragments(columnMetadata.getFullName(join.getAlias()), columnMetadata, column)
                                            .map(fragments -> {
                                                PrepareSqlFragments sqlFragments = PrepareSqlFragments.of().addFragments(fragments);
                                                sqlFragments.addSql("as").addSql(getAlias(join.getAlias(), columnMetadata, column));

                                                return sqlFragments;
                                            })))
                    .orElse(null);
        }

        return metadata
                .findColumn(column.getColumn())
                .flatMap(columnMetadata -> createFragments(columnMetadata.getFullName(), columnMetadata, column)
                        .map(fragments -> {
                            PrepareSqlFragments sqlFragments = PrepareSqlFragments.of().addFragments(fragments);
                            sqlFragments.addSql("as").addSql(getAlias(null, columnMetadata, column));
                            return sqlFragments;
                        }))
                .orElse(null);
    }

    public Optional<SqlFragments> createFragments(String columnFullName, RDBColumnMetadata columnMetadata, SelectColumn column) {
        String function = column.getFunction();
        if (function != null) {
            return columnMetadata
                    .<FunctionFragmentBuilder>findFeature(RDBFeatureType.function.getFeatureId(function))
                    .map(fragment -> fragment.create(columnFullName, columnMetadata, column.getOpts()));
        } else {
            return Optional.of(PrepareSqlFragments.of().addSql(columnFullName));
        }
    }

    @Override
    public String getId() {
        return RDBFutures.select;
    }

    @Override
    public String getName() {
        return "查询列";
    }
}
