package org.hswebframework.ezorm.rdb.operator.builder.fragments.query;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.AbstractTermsFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.ForeignKeyTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

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
                return parameter
                        .findJoin(arr[0]) //先找join的表
                        .flatMap(join -> metaData
                                .getSchema()
                                .getTableOrView(join.getTarget())
                                .flatMap(tableOrView -> tableOrView.getColumn(arr[1]))
                                .flatMap(column -> column
                                        .findFeature(createFeatureId((term.getTermType())))
                                        .map(termFragment -> termFragment.createFragments(createColumnFullName(column,parameter.getFromAlias()), column, term))))
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
        }

        return metaData
                .getColumn(columnName)
                .flatMap(column -> column
                        .findFeature(createFeatureId(term.getTermType()))
                        .map(termFragment -> termFragment.createFragments(createColumnFullName(column,parameter.getFromAlias()), column, term)))
                .orElse(EmptySqlFragments.INSTANCE);

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

    protected String createColumnFullName(RDBColumnMetadata column,String fromAlias){
        return column.getFullName(fromAlias);
    }
}
