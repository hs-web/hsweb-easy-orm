package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.hswebframework.ezorm.rdb.meta.RDBFeatureType.termType;

public class DefaultTermsFragmentBuilder extends AbstractTermsFragmentBuilder<DefaultTermsFragmentBuilder.Alias> implements TermsFragmentBuilder {

    TableOrViewMetadata metadata;

    @Override
    public SqlFragments createFragments(String termPrefix, List<Term> terms, String... tableAlias) {
        Alias alias = new Alias();
        alias.termPrefix = termPrefix;
        alias.tableAlias = new HashSet<>(Arrays.asList(tableAlias));

        return createFragments(alias, terms);
    }

    @Override
    protected SqlFragments createTermFragments(Alias parameter, Term term) {

        String columnName = term.getColumn();


        return metadata
                .findColumn(columnName)
                .flatMap(column -> column
                        .<TermFragmentBuilder>findFeature(termType.getFeatureId(term.getTermType()))
                        .map(termFragment -> termFragment.createFragments(column.getFullName(parameter.termPrefix), column, term)))
                .orElse(EmptySqlFragments.INSTANCE);
    }


    class Alias {
        private String termPrefix;

        private Set<String> tableAlias;
    }
}
