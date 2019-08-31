package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.core.param.SqlTerm;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.utils.RDBUtils;

import java.util.List;

public abstract class AbstractTermsFragmentBuilder<T> {

    protected SqlFragments createFragments(T parameter, List<Term> terms) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();

        int index = 0;
        boolean termAvailable;
        boolean lastTermAvailable = false;
        for (Term term : terms) {

            index++;
            SqlFragments termFragments;

            if (term instanceof SqlTerm) {
                termFragments = PrepareSqlFragments.of()
                        .addSql(((SqlTerm) term).getSql())
                        .addParameter(RDBUtils.convertList(term.getValue()));
            } else {
                termFragments = createTermFragments(parameter, term);
            }

            termAvailable = !termFragments.isEmpty();
            if (termAvailable) {
                if (index != 1 && lastTermAvailable) {
                    //and or
                    fragments.addSql(term.getType().name());
                }
                fragments.addFragments(termFragments);
            }

            List<Term> nest = term.getTerms();
            //嵌套条件
            if (nest != null && !nest.isEmpty()) {
                //递归....
                SqlFragments nestFragments = createFragments(parameter, nest);
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

    protected abstract SqlFragments createTermFragments(T parameter, Term term);

}
