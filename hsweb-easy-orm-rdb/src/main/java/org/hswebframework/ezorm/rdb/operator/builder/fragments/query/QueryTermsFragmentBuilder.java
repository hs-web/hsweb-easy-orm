package org.hswebframework.ezorm.rdb.operator.builder.fragments.query;

import org.apache.commons.collections4.CollectionUtils;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.AbstractTermsFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.ForeignKeyTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;
import org.hswebframework.ezorm.rdb.utils.FlatList;

import java.util.*;

import static org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder.createFeatureId;

public class QueryTermsFragmentBuilder extends AbstractTermsFragmentBuilder<QueryOperatorParameter> implements QuerySqlFragmentBuilder {

    private final TableOrViewMetadata metaData;

    private final Set<String> alias;

    protected QueryTermsFragmentBuilder(TableOrViewMetadata metaData, Set<String> alias) {
        this.metaData = metaData;
        this.alias = alias;
    }

    public static QueryTermsFragmentBuilder of(TableOrViewMetadata metadata) {
        return of(metadata, Collections.emptySet());
    }

    public static QueryTermsFragmentBuilder of(TableOrViewMetadata metaData, Set<String> alias) {
        return new QueryTermsFragmentBuilder(metaData, alias);
    }

    @Override
    public String getId() {
        return where;
    }

    @Override
    public String getName() {
        return "查询条件";
    }


    protected SqlFragments createByColumn(RDBColumnMetadata column, String owner, Term term) {
        if (column != null) {
            TermFragmentBuilder builder = column.findFeature(createFeatureId(term.getTermType())).orElse(null);
            if (builder != null) {
                return builder
                    .createFragments(createColumnFullName(column, owner), column, term);
            }
        }
        return EmptySqlFragments.INSTANCE;
    }


    private SqlFragments createByJoin(String[] arr, QueryOperatorParameter parameter, Term term) {
        return parameter
            .findJoin(arr[0]) //先找join的表
            .flatMap(join -> metaData
                .getSchema()
                .getTableOrView(join.getTarget())
                .flatMap(tableOrView -> tableOrView.getColumn(arr[1]))
                .map(column -> createByColumn(column, join.getAlias(), term)))
            .orElseGet(() -> {//外键关联查询
                return metaData
                    .getForeignKey(arr[0])
                    .flatMap(key -> key
                        .getSource()
                        .findFeature(ForeignKeyTermFragmentBuilder.ID)
                        .map(builder -> builder.createFragments(parameter.getFromAlias(), key, createForeignKeyTerm(key, term))))
                    .orElse(EmptySqlFragments.INSTANCE);
            });
    }

    protected SqlFragments createTermFragments(QueryOperatorParameter parameter, Term term) {
        String columnName = term.getColumn();
        if (columnName == null) {
            return EmptySqlFragments.INSTANCE;
        }

        if (columnName.contains(".")) {
            String[] arr = columnName.split("[.]");
            if (metaData.equalsNameOrAlias(arr[0]) || arr[0].equals(parameter.getFromAlias()) || alias.contains(arr[0])) {
                columnName = arr[1];
            } else {
                return createByJoin(arr, parameter, term);
            }
        }

        RDBColumnMetadata column = metaData.getColumn(columnName).orElse(null);
        if (column != null) {
            return createByColumn(column, parameter.getFromAlias(), term);
        }

        List<SelectColumn> cols = parameter.getSelect();
        if (CollectionUtils.isNotEmpty(parameter.getAlias())) {
            cols = new FlatList<>(Arrays.asList(cols, parameter.getAlias()));
        }
        //匹配查询的列别名
        for (SelectColumn selectColumn : cols) {
            if (Objects.equals(selectColumn.getAlias(), columnName)) {
                String selectColumnName = selectColumn.getColumn();
                //join
                if (selectColumnName.contains(".")) {
                    return createByJoin(selectColumnName.split("[.]"), parameter, term);
                }
                column = metaData.getColumn(columnName).orElse(null);
                if (column != null) {
                    return createByColumn(column, parameter.getFromAlias(), term);
                }
            }
        }
        String cname = columnName;

        //匹配join
        for (Join join : parameter.getJoins()) {
            RDBColumnMetadata joinColumn = metaData
                .getSchema()
                .getTableOrView(join.getTarget())
                .flatMap(joinTable -> joinTable.getColumn(cname))
                .orElse(null);
            if (joinColumn != null) {
                return createByColumn(joinColumn, join.getAlias(), term);
            }
        }

        return EmptySqlFragments.INSTANCE;

    }

    protected List<Term> createForeignKeyTerm(ForeignKeyMetadata keyMetadata, Term term) {
        Term copy = term.clone();
        //只要是嵌套到外键表的条件则认为是关联表的条件
        term.setTerms(new LinkedList<>());

        return Collections.singletonList(copy);
    }

    @Override
    public SqlFragments createFragments(QueryOperatorParameter parameter) {
        return createTermFragments(parameter, parameter.getWhere());
    }

    protected String createColumnFullName(RDBColumnMetadata column, String owner) {
        return column.getFullName(owner);
    }
}
