package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.meta.RDBFutures;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

import static org.hswebframework.ezorm.rdb.meta.RDBFeatureType.*;

@AllArgsConstructor(staticName = "of")
public class WhereFragmentBuilder implements SqlFragmentBuilder {

    private TableOrViewMetadata metaData;

    @Override
    public String getId() {
        return RDBFutures.where;
    }

    @Override
    public String getText() {
        return "条件";
    }

    @Override
    public PrepareSqlFragments createFragments(ComplexQueryParameter parameter) {
        List<Term> terms = parameter.getWhere();
        if (terms == null || terms.isEmpty()) {
            return null;
        }
        List<String> sql = new ArrayList<>(terms.size());
        List<Object> parameters = new ArrayList<>();

        Queue<List<Term>> nests = new LinkedList<>();
        nests.add(terms);

        do {
            List<Term> termList = nests.poll();
            if (termList == null) {
                break;
            }
            sql.add("(");
            int index = 0;

            for (Term term : termList) {
                if (term.getValue() == null) {
                    continue;
                }
                String columnName = term.getColumn();

                if (columnName != null && !columnName.isEmpty()) {
                    SqlFragments fragments = metaData
                            .getColumn(columnName)
                            .flatMap(column -> {
                                String tableAlias = column.getOwner().getName();

                                return column
                                        .<TermFragmentBuilder>findFeature(termType.getFeatureId(term.getTermType()))
                                        .map(termFragment -> termFragment.createFragments(tableAlias, column, term));

                            }).orElse(null);
                    if (fragments == null || fragments.isEmpty()) {

                        continue;
                    }
                    sql.addAll(fragments.getSql());
                    parameters.addAll(fragments.getParameters());
                }

                if (index != 0) {
                    sql.add(term.getType() == Term.Type.or ? "or" : "and");
                }

                List<Term> nest = term.getTerms();
                if (null != nest && !nest.isEmpty()) {
                    nests.add(nest);
                }
                index++;

            }

            sql.add(")");

        } while (!nests.isEmpty());

        PrepareSqlFragments fragment = PrepareSqlFragments.of();

        fragment.setSql(sql);
        fragment.setParameters(parameters);

        return fragment;
    }
}
