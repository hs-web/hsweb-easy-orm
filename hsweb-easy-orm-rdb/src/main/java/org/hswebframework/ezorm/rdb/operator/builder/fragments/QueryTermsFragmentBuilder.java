package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.meta.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBFutures;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.ForeignKeyTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;

import java.util.*;

import static org.hswebframework.ezorm.rdb.meta.RDBFeatureType.*;

@AllArgsConstructor(staticName = "of")
public class QueryTermsFragmentBuilder extends AbstractTermsFragmentBuilder<ComplexQueryParameter> implements QuerySqlFragmentBuilder {

    private TableOrViewMetadata metaData;

    private Set<String> alias;

    public static QueryTermsFragmentBuilder of(TableOrViewMetadata metadata) {
        return of(metadata, Collections.emptySet());
    }

    @Override
    public String getId() {
        return RDBFutures.where;
    }

    @Override
    public String getName() {
        return "条件";
    }

    protected SqlFragments createTermFragments(ComplexQueryParameter parameter, Term term) {
        String columnName = term.getColumn();
        if (columnName == null) {
            return EmptySqlFragments.INSTANCE;
        }

        if (columnName.contains(".")) {
            String[] arr = columnName.split("[.]");
            if (metaData.equalsNameOrAlias(arr[0]) || arr[0].equals(parameter.getFromAlias()) || alias.contains(arr[0])) {
                columnName = arr[1];
            } else {
                return parameter.findJoin(arr[0]) //先找join的表
                        .flatMap(join -> metaData.getSchema()
                                .getTableOrView(join.getTarget())
                                .flatMap(tableOrView -> tableOrView.getColumn(arr[1]))
                                .flatMap(column -> column
                                        .<TermFragmentBuilder>findFeature(termType.getFeatureId(term.getTermType()))
                                        .map(termFragment -> termFragment.createFragments(column.getFullName(join.getAlias()), column, term))))
                        .orElseGet(() -> {//外键关联查询
                            return metaData.getForeignKey(arr[0])
                                    .flatMap(key -> key.getSourceColumn()
                                            .<ForeignKeyTermFragmentBuilder>getFeature(foreignKeyTerm.getId())
                                            .map(builder -> builder.createFragments(key.getName(), key, createForeignKeyTerm(key, term))))
                                    .orElse(EmptySqlFragments.INSTANCE);
                        });
            }
        }

        return metaData
                .getColumn(columnName)
                .flatMap(column -> column
                        .<TermFragmentBuilder>findFeature(termType.getFeatureId(term.getTermType()))
                        .map(termFragment -> termFragment.createFragments(column.getFullName(parameter.getFromAlias()), column, term)))
                .orElse(EmptySqlFragments.INSTANCE);

    }

    protected List<Term> createForeignKeyTerm(ForeignKeyMetadata keyMetadata, Term term) {
        Term copy = term.clone();
        //只要是嵌套到外键表的条件则认为是关联表的条件
        term.setTerms(new LinkedList<>());

        return Collections.singletonList(copy);
    }

    @Override
    public SqlFragments createFragments(ComplexQueryParameter parameter) {
        return createFragments(parameter, parameter.getWhere());
    }
}
