package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;
import org.hswebframework.ezorm.rdb.operator.builder.PrepareSqlFragment;
import org.hswebframework.ezorm.rdb.operator.builder.SqlFragment;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

@AllArgsConstructor(staticName = "of")
public class WhereFragment implements SqlFragment {

    private TableOrViewMetadata metaData;

    @Override
    public PrepareSqlFragment getFragment(ComplexQueryParameter parameter) {
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
                    PrepareSqlFragment fragment = metaData
                            .getColumn(columnName)
                            .flatMap(column -> {
                                String tableAlias = column.getOwner().getName();

                                return metaData.getSchema()
                                        .getDatabase()
                                        .getDialect()
                                        .getTermFragment(term.getTermType())
                                        .map(termFragment -> termFragment.createFragments(tableAlias, column, term));

                            }).orElse(null);
                    if (fragment == null) {
                        continue;
                    }
                    sql.add(fragment.getSql());
                    parameters.addAll(fragment.getParameters());
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

        PrepareSqlFragment fragment=new PrepareSqlFragment();
        fragment.setSql(String.join(" ", sql));
        fragment.setParameters(parameters);

        return fragment;
    }
}
