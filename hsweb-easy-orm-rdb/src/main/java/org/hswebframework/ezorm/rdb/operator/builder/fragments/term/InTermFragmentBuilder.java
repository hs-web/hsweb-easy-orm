package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
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

    public InTermFragmentBuilder(String termType, String name, boolean isNot) {
        this(termType, name, isNot, SPLIT_LARGE);
    }

    public InTermFragmentBuilder(String termType, String name, boolean isNot, boolean splitLargeParameter) {
        super(termType, name);
        not = isNot;
        symbol = isNot ? "not in(" : "in(";
        this.splitLargeParameter = splitLargeParameter;
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {

        List<Object> value = convertList(column, term);
        if (value == null || value.isEmpty()) {
            return EmptySqlFragments.INSTANCE;
        }
        int len = value.size();

        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        if (!splitLargeParameter) {
            String[] arr = new String[len];
            Arrays.fill(arr, "?");
            fragments.addSql(columnFullName)
                     .addSql(symbol)
                     .addSql("(")
                     .addSql(arr)
                     .addSql(")")
                     .addParameter(value);
            return fragments;
        }
        //参数数量大于 500时,使用(column in (?,?,?) or column in(?,?,?))
        if (len > SPLIT_LARGE_SIZE) {
            fragments.addSql("(");
        }
        fragments.addSql(columnFullName)
                 .addSql(symbol);

        int flag = 0;
        for (int i = 0; i < len; i++) {
            if (flag++ != 0) {
                fragments.addSql(",");
            }
            fragments.addSql("?");
            if (flag > SPLIT_LARGE_SIZE && i != len - 1) {
                flag = 0;
                if (not) {
                    fragments.addSql(") and");
                } else {
                    fragments.addSql(") or");
                }
                fragments.addSql(columnFullName)
                         .addSql(symbol);
            }
        }
        if (len > SPLIT_LARGE_SIZE) {
            fragments.addSql(")");
        }
        return fragments
                .addSql(")")
                .addParameter(value);
    }
}
