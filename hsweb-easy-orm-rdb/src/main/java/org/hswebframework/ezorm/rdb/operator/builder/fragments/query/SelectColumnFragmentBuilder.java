package org.hswebframework.ezorm.rdb.operator.builder.fragments.query;

import org.hswebframework.ezorm.core.param.SqlTerm;
import org.hswebframework.ezorm.core.utils.StringUtils;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyColumn;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;

import java.util.*;

import static java.util.Optional.ofNullable;
import static org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder.createFeatureId;

public class SelectColumnFragmentBuilder implements QuerySqlFragmentBuilder {

    private final TableOrViewMetadata metadata;

    protected SelectColumnFragmentBuilder(TableOrViewMetadata metadata) {
        this.metadata = metadata;
    }

    public static SelectColumnFragmentBuilder of(TableOrViewMetadata metadata) {
        return new SelectColumnFragmentBuilder(metadata);
    }

    @Override
    public String getId() {
        return selectColumns;
    }

    @Override
    public String getName() {
        return "查询列";
    }

    private Set<SelectColumn> getAllSelectColumn(String ownerAlias, Set<String> excludes, TableOrViewMetadata metadata) {
        List<RDBColumnMetadata> metadataList = metadata.getColumns();

        Set<SelectColumn> columns = new HashSet<>(metadataList.size());

        for (RDBColumnMetadata column : metadata.getColumns()) {
            SelectColumn selectColumn;
            if (ownerAlias == null) {
                selectColumn = SelectColumn.of(column.getName(), column.getAlias());
            } else {
                selectColumn = SelectColumn.of(
                        StringUtils.concat(ownerAlias, ".", column.getName()),
                        StringUtils.concat(ownerAlias, ".", column.getAlias()));
            }
            if (excludes.contains(selectColumn.getColumn()) || excludes.contains(selectColumn.getAlias())) {
                continue;
            }
            columns.add(selectColumn);
        }

        return columns;

//        return metadata
//                .getColumns()
//                .stream()
//                .map(columnMetadata -> ofNullable(ownerAlias)
//                        .map(alias -> SelectColumn.of(alias.concat(".")
//                                                           .concat(columnMetadata.getName()), alias.concat(".")
//                                                                                                   .concat(columnMetadata.getAlias())))
//                        .orElseGet(() -> SelectColumn.of(columnMetadata.getName(), columnMetadata.getAlias())))
//                .filter(column -> !excludes.contains(column.getColumn()) && !excludes.contains(column.getAlias()))
//                .collect(Collectors.toSet());
    }

    private Set<SelectColumn> createSelectColumns(QueryOperatorParameter parameter) {
        Set<SelectColumn> columns = new LinkedHashSet<>(parameter.getSelect());
        Set<String> excludes = parameter.getSelectExcludes();
        if (columns.isEmpty()) {
            return getAllSelectColumn(null, excludes, metadata);
        } else {
            Set<SelectColumn> realColumns = new LinkedHashSet<>();
            for (SelectColumn column : parameter.getSelect()) {
                String columnName = column.getColumn();
                if (column.getFunction() != null || columnName == null || excludes.contains(columnName)) {
                    realColumns.add(column);
                    continue;
                }
                if (columnName.contains("*")) {
                    String[] arr = columnName.split("[.]");
                    if (arr.length == 1 || metadata.equalsNameOrAlias(arr[0])) {
                        //当前表全部字段
                        realColumns.addAll(getAllSelectColumn(null, excludes, metadata));
                    } else if (arr.length == 2) {
                        //join的表
                        parameter
                                .findJoin(arr[0])
                                .flatMap(join -> metadata.getSchema().findTableOrView(join.getTarget()))
                                .map(tar -> getAllSelectColumn(arr[0], excludes, tar))
                                .map(Optional::of)
                                .orElseGet(() -> {
                                    //逻辑外键表全部字段
                                    return metadata.getForeignKey(arr[0])
                                                   .filter(ForeignKeyMetadata::isAutoJoin)
                                                   .map(ForeignKeyMetadata::getTarget)
                                                   .map(tar -> getAllSelectColumn(arr[0], excludes, tar));
                                })
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

        Set<SelectColumn> columns = createSelectColumns(parameter);

        PrepareSqlFragments main = PrepareSqlFragments.of();

        PrepareSqlFragments sql = null;
        for (SelectColumn column : columns) {
            PrepareSqlFragments sqlNext = this.createFragments(parameter, column);
            if (sqlNext != null && sqlNext.isNotEmpty()) {
                if (sql != null) {
                    main.addSql(",");
                }
                sql = sqlNext;
                main.addFragments(sql);
            }
        }

        return main;
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
            alias = StringUtils.concat(owner, ".", alias);
        }
        return metadata.getDialect().quote(alias, false);

    }

    @SuppressWarnings("all")
    protected List<Join> createJoin(String owner, String target, ForeignKeyMetadata key) {

        List<Join> joins = new ArrayList<>();

        //中间表
        for (ForeignKeyMetadata middleForeignKey : key.getMiddleForeignKeys()) {
            Join join = new Join();

            for (ForeignKeyColumn column : middleForeignKey.getColumns()) {
                PrepareSqlFragments condition = PrepareSqlFragments.of();
                condition.addSql(column.getSourceColumn().getFullName(key.getAlias()));
                condition.addSql("=").addSql(column.getTargetColumn().getFullName(middleForeignKey.getAlias()));
                join.getTerms().add(SqlTerm.of(condition.toRequest().getSql()));
            }

            if (middleForeignKey.getTerms() != null) {
                join.getTerms().addAll(middleForeignKey.getTerms());
            }
            join.setTarget(middleForeignKey.getTarget().getFullName());
            join.setType(middleForeignKey.getJoinType());
            join.addAlias(middleForeignKey.getName(),
                          middleForeignKey.getAlias(),
                          middleForeignKey.getTarget().getAlias(),
                          middleForeignKey.getTarget().getName());
            joins.add(join);
        }

        {
            Join join = new Join();
            join.setType(key.getJoinType());
            join.setTarget(key.getTarget().getFullName());
            join.setAlias(owner);
            join.addAlias(key.getTarget().getAlias());
            //关联条件
            for (ForeignKeyColumn column : key.getColumns()) {
                PrepareSqlFragments condition = PrepareSqlFragments.of();
                condition.addSql(column.getSourceColumn().getFullName(target));
                condition.addSql("=").addSql(column.getTargetColumn().getFullName(key.getAlias()));
                join.getTerms().add(SqlTerm.of(condition.toRequest().getSql()));
            }
            joins.add(join);
        }


        return joins;
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
            return parameter
                    .findJoin(arr[0])
                    .flatMap(join -> metadata
                            .getSchema()
                            .getTableOrView(join.getTarget())
                            .flatMap(table -> table.getColumn(arr[1]))
                            .flatMap(joinColumn -> this
                                    .createFragments(getColumnFullName(joinColumn, join.getAlias()), joinColumn, selectColumn)
                                    .map(fragments -> PrepareSqlFragments
                                            .of()
                                            .addFragments(fragments)
                                            .addSql("as", getAlias(join.getAlias(), joinColumn, selectColumn)))))
                    .orElseGet(() -> metadata
                            .getForeignKey(arr[0])
                            .filter(ForeignKeyMetadata::isAutoJoin) //自动关联查询
                            .filter(key -> key.getTarget().findColumn(arr[1]).isPresent()) //存在这个字段
                            .map(foreignKey -> {
                                //自动join
                                if (!parameter.findJoin(arr[0]).isPresent()) {
                                    parameter
                                            .getJoins()
                                            .addAll(createJoin(arr[0], parameter.getFromAlias(), foreignKey));
                                }
                                PrepareSqlFragments sqlFragments = PrepareSqlFragments.of();
                                RDBColumnMetadata targetColumn = foreignKey
                                        .getTarget()
                                        .getColumn(arr[1])
                                        .orElse(null);
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
        String columnFullName = ofNullable(finalColumnMetadata).map(this::getColumnFullName).orElse(null);

        return this
                .createFragments(columnFullName, columnMetadata, selectColumn)
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
                    .map(fragment -> fragment.create(columnFullName, columnMetadata, column))
                    .map(fragment -> {
                        if (fragment.isEmpty()) {
                            throw new UnsupportedOperationException("unsupported function:" + column);
                        }
                        return fragment;
                    });
        } else {
            return ofNullable(columnFullName)
                    .map(PrepareSqlFragments::of);
        }
    }

    protected String getColumnFullName(RDBColumnMetadata column, String alias) {
        return column.getFullName(alias);
    }

    protected String getColumnFullName(RDBColumnMetadata column) {
        return column.getFullName();
    }

}
