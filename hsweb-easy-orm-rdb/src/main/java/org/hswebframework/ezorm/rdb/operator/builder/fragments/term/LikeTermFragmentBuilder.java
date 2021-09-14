package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

public class LikeTermFragmentBuilder extends AbstractTermFragmentBuilder {
    private final boolean not;

    public LikeTermFragmentBuilder(boolean not) {
        super(not ? TermType.nlike : TermType.like, not ? "Not Like" : "Like");
        this.not = not;
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {

        boolean reversal = term.getOptions().contains("reversal");
        boolean startWith = term.getOptions().contains("startWith");
        boolean endWith = term.getOptions().contains("endWith");

        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        if (reversal) {
            fragments.addSql("?").addParameter(term.getValue());
        }else {
            fragments.addSql(columnFullName);
        }
        if (not) {
            fragments.addSql("not");
        }
        fragments.addSql("like");

        if (reversal) {
            if (startWith) {
                fragments.addSql("concat( '%',");
            } else {
                fragments.addSql("concat(");
            }
            fragments.addSql(columnFullName);
            if (endWith) {
                fragments.addSql(",'%' )");
            } else {
                fragments.addSql(")");
            }
        } else {
            fragments.addSql("?").addParameter(term.getValue());
        }
        return fragments;
    }
}
