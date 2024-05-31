package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BatchSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

public class LikeTermFragmentBuilder extends AbstractTermFragmentBuilder {
    private final boolean not;

    static final SqlFragments LIKE = SqlFragments.of("like"),
        CONCAT_L = SqlFragments.of("concat( '%',"),
        CONCAT_R = SqlFragments.of(",'%' )"),
        CONCAT = SqlFragments.of("concat(");

    public LikeTermFragmentBuilder(boolean not) {
        super(not ? TermType.nlike : TermType.like, not ? "Not Like" : "Like");
        this.not = not;
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {

        boolean reversal = term.getOptions().contains("reversal");
        boolean startWith = term.getOptions().contains("startWith");
        boolean endWith = term.getOptions().contains("endWith");
        BatchSqlFragments fragments = new BatchSqlFragments(not ? 4 : 3, 1);
        if (reversal) {
            fragments.add(SqlFragments.QUESTION_MARK).addParameter(term.getValue());
        } else {
            fragments.addSql(columnFullName);
        }
        if (not) {
            fragments.add(SqlFragments.NOT);
        }
        fragments.add(LIKE);

        if (reversal) {
            if (startWith) {
                fragments.add(CONCAT_L);
            } else {
                fragments.add(CONCAT);
            }
            fragments.addSql(columnFullName);
            if (endWith) {
                fragments.add(CONCAT_R);
            } else {
                fragments.add(SqlFragments.RIGHT_BRACKET);
            }
        } else {
            fragments.add(SqlFragments.QUESTION_MARK)
                     .addParameter(term.getValue());
        }
        return fragments;
    }
}
