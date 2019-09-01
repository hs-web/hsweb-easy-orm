package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.param.SqlTerm;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.utils.RDBUtils;

import java.util.List;

@SuppressWarnings("all")
public abstract class AbstractTermsFragmentBuilder<T> {

    @Setter
    @Getter
    private boolean useBlock = false;

    private BlockSqlFragments createBlockFragments(T parameter, List<Term> terms) {
        BlockSqlFragments fragments = BlockSqlFragments.of();

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

            termAvailable = termFragments.isNotEmpty();
            if (termAvailable) {
                BlockSqlFragments termBlock = BlockSqlFragments.of();

                if (index != 1 && lastTermAvailable) {
                    //and or
                    termBlock.addBlock(FragmentBlock.before, term.getType().name());
                }
                termBlock.addBlock(FragmentBlock.term, termFragments);
                fragments.addBlock(FragmentBlock.term, termBlock);
            }
            BlockSqlFragments nestBlock = BlockSqlFragments.of();

            List<Term> nest = term.getTerms();
            //嵌套条件
            if (nest != null && !nest.isEmpty()) {
                //递归....
                SqlFragments nestFragments = createFragments(parameter, nest);
                if (nestFragments.isNotEmpty()) {
                    //and or
                    if (termAvailable || lastTermAvailable) {
                        nestBlock.addBlock(FragmentBlock.before, term.getType().name());
                    }
                    nestBlock.addBlock(FragmentBlock.before, "(");
                    nestBlock.addBlock(FragmentBlock.term, nestFragments);
                    nestBlock.addBlock(FragmentBlock.after, ")");

                    fragments.addBlock(FragmentBlock.term, nestBlock);
                    lastTermAvailable = true;
                    continue;
                }
            }
            lastTermAvailable = termAvailable;

        }

        return fragments;
    }

    private PrepareSqlFragments createPrepareFragments(T parameter, List<Term> terms) {
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

            termAvailable = termFragments.isNotEmpty();
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
                if (nestFragments.isNotEmpty()) {
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

    protected SqlFragments createFragments(T parameter, List<Term> terms) {
        return isUseBlock() ? createBlockFragments(parameter, terms) : createPrepareFragments(parameter, terms);
    }

    protected abstract SqlFragments createTermFragments(T parameter, Term term);

}
