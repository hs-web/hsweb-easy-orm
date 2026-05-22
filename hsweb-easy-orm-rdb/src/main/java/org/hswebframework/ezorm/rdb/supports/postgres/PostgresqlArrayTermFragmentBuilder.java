package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.AbstractTermFragmentBuilder;

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.List;

public class PostgresqlArrayTermFragmentBuilder extends AbstractTermFragmentBuilder {

    public static final PostgresqlArrayTermFragmentBuilder in = new PostgresqlArrayTermFragmentBuilder(
        TermType.in,
        "数组包含任一值",
        null,
        false
    );

    public static final PostgresqlArrayTermFragmentBuilder notIn = new PostgresqlArrayTermFragmentBuilder(
        TermType.nin,
        "数组不包含任一值",
        null,
        true
    );

    public static final PostgresqlArrayTermFragmentBuilder contains = new PostgresqlArrayTermFragmentBuilder(
        TermType.contains,
        "数组包含",
        Operator.contains,
        false
    );

    public static final PostgresqlArrayTermFragmentBuilder notContains = new PostgresqlArrayTermFragmentBuilder(
        TermType.ncontains,
        "数组不包含",
        Operator.contains,
        true
    );

    public static final PostgresqlArrayTermFragmentBuilder contained = new PostgresqlArrayTermFragmentBuilder(
        TermType.contained,
        "数组被包含",
        Operator.contained,
        false
    );

    public static final PostgresqlArrayTermFragmentBuilder notContained = new PostgresqlArrayTermFragmentBuilder(
        TermType.ncontained,
        "数组不被包含",
        Operator.contained,
        true
    );

    public static final PostgresqlArrayTermFragmentBuilder overlap = new PostgresqlArrayTermFragmentBuilder(
        TermType.overlap,
        "数组相交",
        Operator.overlap,
        false
    );

    public static final PostgresqlArrayTermFragmentBuilder notOverlap = new PostgresqlArrayTermFragmentBuilder(
        TermType.noverlap,
        "数组不相交",
        Operator.overlap,
        true
    );

    private final Operator operator;

    private final boolean not;

    public PostgresqlArrayTermFragmentBuilder(String termType,
                                              String name,
                                              Operator operator,
                                              boolean not) {
        super(termType, name);
        this.operator = operator;
        this.not = not;
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {
        Object value = term.getValue();
        if (isEmptyValue(value)) {
            return EmptySqlFragments.INSTANCE;
        }
        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        if (not) {
            fragments.addSql("not", "(");
        }
        fragments.addSql(columnFullName, resolveOperator(term).sql);
        appendPrepareOrNative(fragments, encodeValue(column, value));
        if (not) {
            fragments.addSql(")");
        }
        return fragments;
    }

    private Object encodeValue(RDBColumnMetadata column, Object value) {
        if (value instanceof NativeSql) {
            return value;
        }
        return column.encode(value);
    }

    private Operator resolveOperator(Term term) {
        if (operator != null) {
            return operator;
        }
        List<String> options = term.getOptions();
        if (options.contains("all") || options.contains("contains")) {
            return Operator.contains;
        }
        if (options.contains("contained")) {
            return Operator.contained;
        }
        return Operator.overlap;
    }

    private boolean isEmptyValue(Object value) {
        if (value == null) {
            return true;
        }
        if (value instanceof Collection<?> collection) {
            return collection.isEmpty();
        }
        if (value.getClass().isArray()) {
            return Array.getLength(value) == 0;
        }
        return false;
    }

    private enum Operator {
        contains("@>"),
        contained("<@"),
        overlap("&&");

        private final String sql;

        Operator(String sql) {
            this.sql = sql;
        }
    }
}
