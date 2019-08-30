package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.meta.RDBFutures;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.ForeignKeyTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;

import java.util.Collections;
import java.util.List;

import static org.hswebframework.ezorm.rdb.meta.RDBFeatureType.*;

@AllArgsConstructor(staticName = "of")
public class WhereFragmentBuilder implements QuerySqlFragmentBuilder {

    private TableOrViewMetadata metaData;

    @Override
    public String getId() {
        return RDBFutures.where;
    }

    @Override
    public String getName() {
        return "条件";
    }

    private PrepareSqlFragments createFragments(ComplexQueryParameter parameter, List<Term> terms) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();

        int index = 0;
        boolean termAvailable;
        for (Term term : terms) {

            List<Term> nest = term.getTerms();

            SqlFragments termFragments = createTermFragments(parameter, term);
            termAvailable = termFragments != null && !termFragments.isEmpty();
            if (termAvailable) {
                if (index != 0) {
                    //and or
                    fragments.addSql(term.getType().name());
                }
                fragments.addFragments(termFragments);
            }
            if (nest != null && !nest.isEmpty()) {
                PrepareSqlFragments nestFragments = createFragments(parameter, nest);
                //嵌套
                if (!nestFragments.isEmpty()) {
                    //and or
                    fragments.addSql(term.getType().name());
                    fragments.addSql("(");
                    fragments.addFragments(nestFragments);
                    fragments.addSql(")");
                }
            }
            index++;
        }

        return fragments;
    }


    private SqlFragments createTermFragments(ComplexQueryParameter parameter, Term term) {
        String columnName = term.getColumn();
        if (columnName == null) {
            return EmptySqlFragments.INSTANCE;
        }

        if (columnName.contains(".")) {
            String[] arr = columnName.split("[.]");
            //先找join的表
          return parameter.findJoin(arr[0])
                    .flatMap(join -> metaData.getSchema()
                            .getTableOrView(join.getTarget())
                            .flatMap(tableOrView -> tableOrView.getColumn(arr[1]))
                            .flatMap(column -> column
                                    .<TermFragmentBuilder>findFeature(termType.getFeatureId(term.getTermType()))
                                    .map(termFragment -> termFragment.createFragments(join.getAlias(), column, term))))
                    .orElseGet(() -> {
                        //外键关联查询
                        return metaData.getForeignKey(arr[0])
                                .flatMap(key -> key.getSourceColumn()
                                        .<ForeignKeyTermFragmentBuilder>getFeature(foreignKeyTerm.getId())
                                        .map(builder -> builder.createFragments(key.getName(), key, Collections.singletonList(term))))
                                .orElse(EmptySqlFragments.INSTANCE);
                    });
        }

        return metaData
                .findColumn(columnName)
                .flatMap(column -> {
                    String tableAlias = column.getOwner().getName();

                    return column
                            .<TermFragmentBuilder>findFeature(termType.getFeatureId(term.getTermType()))
                            .map(termFragment -> termFragment.createFragments(tableAlias, column, term));

                }).orElse(EmptySqlFragments.INSTANCE);

    }

    @Override
    public PrepareSqlFragments createFragments(ComplexQueryParameter parameter) {
        return createFragments(parameter, parameter.getWhere());
    }
}
