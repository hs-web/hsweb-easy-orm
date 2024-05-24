package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;

import java.util.List;

import static org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder.createFeatureId;


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
        if (term.getValue() instanceof NativeSql) {
            NativeSql sql = ((NativeSql) term.getValue());
            return PrepareSqlFragments.of(sql.getSql(), sql.getParameters());
        }
        RDBColumnMetadata column = table.getColumn(term.getColumn()).orElse(null);
        if (column == null) {
            return EmptySqlFragments.INSTANCE;
        }

        TermFragmentBuilder builder = column.findFeature(createFeatureId(term.getTermType())).orElse(null);

        if (builder != null) {
            return builder
                .createFragments(column.getQuoteName(), column, term);
        }

        return EmptySqlFragments.INSTANCE;
    }
}
