package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import java.util.ArrayList;
import java.util.List;

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
        boolean ignoreCase = term.getOptions().contains("ignoreCase");
        Dialect dialect = column == null ? Dialect.H2 : column.getDialect();

        SqlFragments left;
        SqlFragments right;
        if (reversal) {
            left = PrepareSqlFragments.of()
                .add(SqlFragments.QUESTION_MARK)
                .addParameter(term.getValue());
            right = createReversalPattern(columnFullName, dialect, startWith, endWith);
        } else {
            left = SqlFragments.of(columnFullName);
            right = PrepareSqlFragments.of()
                .add(SqlFragments.QUESTION_MARK)
                .addParameter(term.getValue());
        }
        return dialect.buildLike(left, right, not, ignoreCase);
    }

    private SqlFragments createReversalPattern(String columnFullName,
                                                Dialect dialect,
                                                boolean startWith,
                                                boolean endWith) {
        List<SqlFragments> expressions = new ArrayList<>(3);
        if (startWith) {
            expressions.add(SqlFragments.of("'%'"));
        }
        expressions.add(SqlFragments.of(columnFullName));
        if (endWith) {
            expressions.add(SqlFragments.of("'%'"));
        }
        return dialect.buildConcat(expressions.toArray(new SqlFragments[0]));
    }
}
