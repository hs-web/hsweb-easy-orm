package org.hswebframework.ezorm.rdb.operator.builder.fragments.query;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.core.param.SqlTerm;
import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;

import java.util.*;
import java.util.stream.Collectors;

import static java.util.Optional.*;
import static org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder.*;

@AllArgsConstructor(staticName = "of")
public class SelectColumnFragmentBuilder implements QuerySqlFragmentBuilder {

    private TableOrViewMetadata metadata;

    @Override
    public String getId() {
        return selectColumns;
    }

    @Override
    public String getName() {
        return "查询列";
    }

    private List<SelectColumn> getAllSelectColumn(String ownerAlias, Set<String> excludes, TableOrViewMetadata metadata) {
        return metadata
                .getColumns()
                .stream()
                .map(columnMetadata ->
                        ofNullable(ownerAlias)
                                .map(alias -> SelectColumn.of(alias.concat(".").concat(columnMetadata.getName()), alias.concat(".").concat(columnMetadata.getAlias())))
                                .orElseGet(() -> SelectColumn.of(columnMetadata.getName(), columnMetadata.getAlias())))
                .filter(column -> !excludes.contains(column.getColumn()) && !excludes.contains(column.getAlias()))
                .collect(Collectors.toList());
    }

    private List<SelectColumn> createSelectColumns(QueryOperatorParameter parameter) {
        List<SelectColumn> columns = parameter.getSelect();
        Set<String> excludes = parameter.getSelectExcludes();
        if (columns.isEmpty()) {
            return getAllSelectColumn(null, excludes, metadata);
        } else {
            List<SelectColumn> realColumns = new ArrayList<>();
            for (SelectColumn column : parameter.getSelect()) {
                String columnName = column.getColumn();
                if (column.getFunction() != null || columnName == null || excludes.contains(columnName)) {
                    realColumns.add(column);
                    continue;
                }
                if (columnName.contains("*")) {
                    String[] arr = columnName.split("[.]");
                    if (arr.length == 1) {
                        //当前表全部字段
                        realColumns.addAll(getAllSelectColumn(null, excludes, metadata));
                    } else if (arr.length == 2) {
                        //逻辑外键表全部字段
                        metadata.getForeignKey(arr[0])
                                .filter(ForeignKeyMetadata::isAutoJoin)
                                .map(ForeignKeyMetadata::getTarget)
                                .map(tar -> getAllSelectColumn(arr[0], excludes, tar))
                                .ifPresent(realColumns::addAll);
                    }

                    continue;
                }
                realColumns.add(column);
            }
            return realColumns;
        }
    }

    @Override
    public SqlFragments createFragments(QueryOperatorParameter parameter) {

        List<SelectColumn> columns = createSelectColumns(parameter);

        PrepareSqlFragments fragments = columns.stream()
                .map(column -> this.createFragments(parameter, column))
                .filter(Objects::nonNull)
                .reduce(PrepareSqlFragments.of(), (main, source) -> main.addFragments(source).addSql(","));

        fragments.removeLastSql();

        return fragments;
    }

    private String getAlias(String owner, RDBColumnMetadata metadata, SelectColumn column) {

        if (column.getAlias() != null) {
            return this.metadata.getDialect().quote(column.getAlias(), false);
        }
        if (metadata == null) {
            return null;
        }

        String alias = metadata.getAlias();

        if (alias.contains(".")) {
            return alias;
        }
        if (owner != null) {
            return owner.concat(".").concat(metadata.getDialect().quote(alias, false));
        }
        return metadata.getDialect().quote(alias, false);

    }

    protected Join createJoin(String owner, String target, ForeignKeyMetadata key) {
        SqlTerm on = new SqlTerm(key.getTargetColumn().getFullName(owner) +
                " = " +
                key.getSourceColumn().getFullName(target));
        if (key.getTerms() != null) {
            on.getTerms().addAll(key.getTerms());
        }
        Join join = new Join();
        join.setType(key.getJoinType());
        join.setTarget(key.getTarget().getFullName());
        join.setAlias(owner);
        join.addAlias(key.getTarget().getAlias());
        join.setTerms(Collections.singletonList(on));

        return join;
    }

    public PrepareSqlFragments createFragments(QueryOperatorParameter parameter, SelectColumn selectColumn) {
        if (selectColumn instanceof NativeSql) {
            return PrepareSqlFragments.of()
                    .addSql(((NativeSql) selectColumn).getSql())
                    .addParameter(((NativeSql) selectColumn).getParameters());
        }
        String columnStr = selectColumn.getColumn();
        RDBColumnMetadata columnMetadata;

        if (columnStr != null && columnStr.contains(".")) {//关联表 table.column
            String[] arr = columnStr.split("[.]");
            return parameter.findJoin(arr[0])
                    .flatMap(join -> metadata.getSchema()
                            .getTableOrView(join.getTarget())
                            .flatMap(table -> table.getColumn(arr[1]))
                            .flatMap(joinColumn ->
                                    createFragments(joinColumn.getFullName(join.getAlias()), joinColumn, selectColumn)
                                            .map(fragments -> PrepareSqlFragments.of()
                                                    .addFragments(fragments)
                                                    .addSql("as", getAlias(join.getAlias(), joinColumn, selectColumn)))))
                    .orElseGet(() ->
                            metadata.getForeignKey(arr[0])
                                    .filter(ForeignKeyMetadata::isAutoJoin) //自动关联查询
                                    .filter(key -> key.getTarget().findColumn(arr[1]).isPresent()) //存在这个字段
                                    .map(foreignKey -> {
                                        //自动join
                                        if (!parameter.findJoin(arr[0]).isPresent()) {
                                            Join join = createJoin(arr[0], parameter.getFromAlias(), foreignKey);
                                            // TODO: 2019-09-06 关联外键处理
                                            parameter.getJoins().add(join);
                                        }
                                        PrepareSqlFragments sqlFragments = PrepareSqlFragments.of();
                                        RDBColumnMetadata targetColumn = foreignKey.getTarget().getColumn(arr[1]).orElse(null);
                                        if (targetColumn == null) {
                                            return null;
                                        }
                                        sqlFragments.addSql(targetColumn.getFullName(arr[0]), "as", getAlias(arr[0], targetColumn, selectColumn));

                                        return sqlFragments;
                                    }).orElse(null));
        } else {
            columnMetadata = metadata.findColumn(columnStr).orElse(null);
        }

        RDBColumnMetadata finalColumnMetadata = columnMetadata;
        String columnFullName = ofNullable(finalColumnMetadata).map(RDBColumnMetadata::getFullName).orElse(null);

        return this.createFragments(columnFullName, columnMetadata, selectColumn)
                .map(fragments -> {
                    PrepareSqlFragments sqlFragments = PrepareSqlFragments.of().addFragments(fragments);
                    sqlFragments.addSql("as").addSql(getAlias(null, finalColumnMetadata, selectColumn));
                    return sqlFragments;
                }).orElse(null);
    }

    public Optional<SqlFragments> createFragments(String columnFullName, RDBColumnMetadata columnMetadata, SelectColumn column) {
        String function = column.getFunction();
        if (function != null) {
            return metadata
                    .findFeature(createFeatureId(function))
                    .map(fragment -> fragment.create(columnFullName, columnMetadata, column.getOpts()));
        } else {
            return ofNullable(columnFullName)
                    .map(PrepareSqlFragments::of);
        }
    }

}
