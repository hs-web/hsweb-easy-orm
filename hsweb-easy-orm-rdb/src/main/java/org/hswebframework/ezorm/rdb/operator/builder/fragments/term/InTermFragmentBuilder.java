package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BatchSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import java.util.Arrays;
import java.util.List;

public class InTermFragmentBuilder extends AbstractTermFragmentBuilder {

    private static final boolean SPLIT_LARGE = Boolean.parseBoolean(System.getProperty("easyorm.term.in.split-large-parameter", "true"));

    private static final int SPLIT_LARGE_SIZE = Integer.getInteger("easyorm.term.in.split-large-size", 500);

    private final String symbol;

    private final boolean not;

    private final boolean splitLargeParameter;

    private final SqlFragments SYMBOL;

    public InTermFragmentBuilder(String termType, String name, boolean isNot) {
        this(termType, name, isNot, SPLIT_LARGE);
    }

    public InTermFragmentBuilder(String termType, String name, boolean isNot, boolean splitLargeParameter) {
        super(termType, name);
        not = isNot;
        symbol = isNot ? "not in(" : "in(";
        this.splitLargeParameter = splitLargeParameter;
        this.SYMBOL = SqlFragments.single(symbol);
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {

        List<Object> value = convertList(column, term);
        if (value == null || value.isEmpty()) {
            return EmptySqlFragments.INSTANCE;
        }
        int len = value.size();

        if (!splitLargeParameter || len < SPLIT_LARGE_SIZE) {
            BatchSqlFragments fragments = new BatchSqlFragments(5, 1);
            fragments.addSql(columnFullName)
                     .add(SYMBOL)
                     .add(createQuestionMarks(len))
                     .add(SqlFragments.RIGHT_BRACKET)
                     .addParameter(value);
            return fragments;
        }
        BatchSqlFragments fragments = new BatchSqlFragments(len * 4, 1);
        //参数数量大于 500时,使用(column in (?,?,?) or column in(?,?,?))
        if (len > SPLIT_LARGE_SIZE) {
            fragments.add(SqlFragments.LEFT_BRACKET);
        }
        fragments
            .addSql(columnFullName)
            .addSql(symbol);

        int flag = 0;
        for (int i = 0; i < len; i++) {
            if (flag++ != 0) {
                fragments.add(SqlFragments.COMMA);
            }
            fragments.add(SqlFragments.QUESTION_MARK);
            if (flag > SPLIT_LARGE_SIZE && i != len - 1) {
                flag = 0;
                if (not) {
                    fragments
                        .add(SqlFragments.RIGHT_BRACKET)
                        .add(SqlFragments.AND);
                } else {
                    fragments
                        .add(SqlFragments.RIGHT_BRACKET)
                        .add(SqlFragments.OR);
                }
                fragments.addSql(columnFullName, symbol);
            }
        }
        if (len > SPLIT_LARGE_SIZE) {
            fragments.add(SqlFragments.RIGHT_BRACKET);
        }
        return fragments
            .add(SqlFragments.RIGHT_BRACKET)
            .addParameter(value);
    }
}
