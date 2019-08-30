package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.core.param.SqlTerm;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.meta.RDBFutures;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.ForeignKeyTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.utils.RDBUtils;

import java.util.*;

import static org.hswebframework.ezorm.rdb.meta.RDBFeatureType.*;

@AllArgsConstructor(staticName = "of")
public class WhereFragmentBuilder implements QuerySqlFragmentBuilder {

    private TableOrViewMetadata metaData;

    private Set<String> alias;

    public static WhereFragmentBuilder of(TableOrViewMetadata metadata) {
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

    private PrepareSqlFragments createFragments(ComplexQueryParameter parameter, List<Term> terms) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();

        int index = 0;
        boolean termAvailable;
        boolean lastTermAvailable = false;
        for (Term term : terms) {
            List<Term> nest = term.getTerms();
            index++;
            SqlFragments termFragments = createTermFragments(parameter, term);
            termAvailable = termFragments != null && !termFragments.isEmpty();
            if (termAvailable) {
                if (index != 1) {
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
                    if (termAvailable || lastTermAvailable) {
                        fragments.addSql(term.getType().name());
                    }
                    fragments.addSql("(");
                    fragments.addFragments(nestFragments);
                    fragments.addSql(")");
                    lastTermAvailable = true;
                    continue;
                }
            }
            lastTermAvailable = termAvailable;

        }

        return fragments;
    }


    private SqlFragments createTermFragments(ComplexQueryParameter parameter, Term term) {
        if (term instanceof SqlTerm) {
            return PrepareSqlFragments.of()
                    .addSql(((SqlTerm) term).getSql())
                    .addParameter(RDBUtils.convertList(term.getValue()));
        }
        String columnName = term.getColumn();
        if (columnName == null) {
            return EmptySqlFragments.INSTANCE;
        }

        if (columnName.contains(".")) {
            String[] arr = columnName.split("[.]");
            if (metaData.equalsNameOrAlias(arr[0]) || alias.contains(arr[0])) {
                columnName = arr[1];
            } else {
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
                                            .map(builder -> builder.createFragments(key.getName(), key, prepareForeignKeyTerm(arr[0], term))))
                                    .orElse(EmptySqlFragments.INSTANCE);
                        });
            }
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

    private List<Term> prepareForeignKeyTerm(String prefix, Term term) {
//        Term copy = term.clone();
//
//        List<Term> nest = term.getTerms();
//
//        if (nest != null && !nest.isEmpty()) {
//
//            List<Term> readyToRemove = new ArrayList<>();
//            for (Term nestTerm : nest) {
//                if (String.valueOf(nestTerm.getColumn()).startsWith(prefix.concat("."))) {
//                    readyToRemove.add(nestTerm);
//                }
//            }
//            nest.removeAll(readyToRemove);
//        }
//        copy.setTerms(nest);

        return Collections.singletonList(term);
    }

    @Override
    public PrepareSqlFragments createFragments(ComplexQueryParameter parameter) {
        return createFragments(parameter, parameter.getWhere());
    }
}
