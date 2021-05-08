package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;

import java.util.List;


public class SimpleTermsFragmentBuilder extends AbstractTermsFragmentBuilder<TableOrViewMetadata> {

    private static final SimpleTermsFragmentBuilder INSTANCE = new SimpleTermsFragmentBuilder();

    private SimpleTermsFragmentBuilder() {

    }

    public static SimpleTermsFragmentBuilder instance() {
        return INSTANCE;
    }

    @Override
    public SqlFragments createTermFragments(TableOrViewMetadata parameter, List<Term> terms) {
        return super.createTermFragments(parameter, terms);
    }

    @Override
    protected SqlFragments createTermFragments(TableOrViewMetadata table, Term term) {
        if(term.getValue() instanceof NativeSql){
            NativeSql sql= ((NativeSql) term.getValue());
            return PrepareSqlFragments.of(sql.getSql(),sql.getParameters());
        }
        return table
                .getColumn(term.getColumn())
                .flatMap(column -> column
                        .findFeature(TermFragmentBuilder.createFeatureId(term.getTermType()))
                        .map(termFragment -> termFragment.createFragments(column.getQuoteName(), column, term)))
                .orElse(EmptySqlFragments.INSTANCE);
    }
}
