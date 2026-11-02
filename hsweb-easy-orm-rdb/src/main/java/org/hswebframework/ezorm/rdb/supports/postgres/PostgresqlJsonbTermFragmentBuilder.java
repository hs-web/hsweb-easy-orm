package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BatchSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.AbstractTermFragmentBuilder;

import java.util.List;

public class PostgresqlJsonbTermFragmentBuilder extends AbstractTermFragmentBuilder {

    public static final PostgresqlJsonbTermFragmentBuilder in = new PostgresqlJsonbTermFragmentBuilder(
        TermType.in,
        "jsonb包含任一键",
        false
    );

    public static final PostgresqlJsonbTermFragmentBuilder notIn = new PostgresqlJsonbTermFragmentBuilder(
        TermType.nin,
        "jsonb不包含任一键",
        true
    );

    public static final PostgresqlJsonbTermFragmentBuilder contains = new PostgresqlJsonbTermFragmentBuilder(
        TermType.contains,
        "jsonb包含",
        false
    );

    public static final PostgresqlJsonbTermFragmentBuilder notContains = new PostgresqlJsonbTermFragmentBuilder(
        TermType.ncontains,
        "jsonb不包含",
        true
    );

    public static final PostgresqlJsonbTermFragmentBuilder contained = new PostgresqlJsonbTermFragmentBuilder(
        TermType.contained,
        "jsonb被包含",
        false
    );

    public static final PostgresqlJsonbTermFragmentBuilder notContained = new PostgresqlJsonbTermFragmentBuilder(
        TermType.ncontained,
        "jsonb不被包含",
        true
    );

    public static final PostgresqlJsonbTermFragmentBuilder overlap = new PostgresqlJsonbTermFragmentBuilder(
        TermType.overlap,
        "jsonb包含任一键",
        false
    );

    public static final PostgresqlJsonbTermFragmentBuilder notOverlap = new PostgresqlJsonbTermFragmentBuilder(
        TermType.noverlap,
        "jsonb不包含任一键",
        true
    );

    private final boolean not;

    public PostgresqlJsonbTermFragmentBuilder(String termType, String name, boolean not) {
        super(termType, name);
        this.not = not;
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {
        SqlFragments fragments = PostgresqlJsonbExistTermFragmentBuilder.createFragments(
            columnFullName,
            column,
            toJsonbOperation(term),
            term.getValue()
        );
        if (fragments.isEmpty() || !not) {
            return fragments;
        }
        return new BatchSqlFragments(7, 1)
            .add(SqlFragments.LEFT_BRACKET)
            .addSql(columnFullName, "is null")
            .add(SqlFragments.OR)
            .addSql("not")
            .add(SqlFragments.LEFT_BRACKET)
            .add(fragments)
            .add(SqlFragments.RIGHT_BRACKET)
            .add(SqlFragments.RIGHT_BRACKET);
    }

    private JsonbOperation toJsonbOperation(Term term) {
        List<String> options = term.getOptions();
        if (options.contains(PostgresqlJsonbExistTermFragmentBuilder.Options.key)) {
            return JsonbOperation.exists;
        }
        if (options.contains(PostgresqlJsonbExistTermFragmentBuilder.Options.all)) {
            return JsonbOperation.existsAll;
        }
        if (options.contains(PostgresqlJsonbExistTermFragmentBuilder.Options.any) ||
            options.contains(PostgresqlJsonbExistTermFragmentBuilder.Options.keys)) {
            return JsonbOperation.existsAny;
        }
        if (options.contains(PostgresqlJsonbExistTermFragmentBuilder.Options.contained)) {
            return JsonbOperation.contained;
        }
        if (options.contains(PostgresqlJsonbExistTermFragmentBuilder.Options.contains) ||
            options.contains(PostgresqlJsonbExistTermFragmentBuilder.Options.json)) {
            return JsonbOperation.contains;
        }
        return switch (term.getTermType()) {
            case TermType.contained, TermType.ncontained -> JsonbOperation.contained;
            case TermType.in, TermType.nin, TermType.overlap, TermType.noverlap -> JsonbOperation.existsAny;
            default -> JsonbOperation.contains;
        };
    }

    enum JsonbOperation {
        exists,
        existsAll,
        existsAny,
        contains,
        contained
    }
}
