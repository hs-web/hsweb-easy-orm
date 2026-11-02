package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.supports.json.*;
import org.hswebframework.ezorm.rdb.utils.SqlUtils;

import java.util.Collections;
import java.util.List;

public class PostgresqlJsonTermFragmentBuilder extends AbstractJsonTermFragmentBuilder {

    public static final PostgresqlJsonTermFragmentBuilder exists =
        new PostgresqlJsonTermFragmentBuilder(JsonTermType.exists, "json路径存在", Operation.exists, false);

    public static final PostgresqlJsonTermFragmentBuilder notExists =
        new PostgresqlJsonTermFragmentBuilder(JsonTermType.notExists, "json路径不存在", Operation.exists, true);

    public static final PostgresqlJsonTermFragmentBuilder contains =
        new PostgresqlJsonTermFragmentBuilder(JsonTermType.contains, "json包含", Operation.contains, false);

    public static final PostgresqlJsonTermFragmentBuilder notContains =
        new PostgresqlJsonTermFragmentBuilder(JsonTermType.notContains, "json不包含", Operation.contains, true);

    public static final PostgresqlJsonTermFragmentBuilder contained =
        new PostgresqlJsonTermFragmentBuilder(JsonTermType.contained, "json被包含", Operation.contained, false);

    public static final PostgresqlJsonTermFragmentBuilder notContained =
        new PostgresqlJsonTermFragmentBuilder(JsonTermType.notContained, "json不被包含", Operation.contained, true);

    public static final PostgresqlJsonTermFragmentBuilder value =
        new PostgresqlJsonTermFragmentBuilder(JsonTermType.value, "json值查询", Operation.value, false);

    public PostgresqlJsonTermFragmentBuilder(String termType, String name, Operation operation, boolean not) {
        super(termType, name, operation, not);
    }

    public static void addJsonFeatures(RDBColumnMetadata column) {
        AbstractJsonTermFragmentBuilder.addJsonFeatures(
            column,
            exists,
            notExists,
            contains,
            notContains,
            contained,
            notContained,
            value
        );
    }

    @Override
    protected SqlFragments createExistsFragments(String columnFullName,
                                                 RDBColumnMetadata column,
                                                 Object path,
                                                 boolean not) {
        List<String> segments = JsonPathUtils.segments(path);
        if (segments.isEmpty()) {
            return SqlFragments.of(columnFullName, not ? "is null" : "is not null");
        }
        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        fragments.addSql("(", columnFullName, "::jsonb", "#>", pathArraySql(segments), ")", not ? "is null" : "is not null")
                 .addParameter(segments);
        return fragments;
    }

    @Override
    protected SqlFragments createContainsFragments(String columnFullName,
                                                   RDBColumnMetadata column,
                                                   Object value,
                                                   boolean not) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        if (not) {
            fragments.addSql("not", "(", columnFullName, "::jsonb", "@>");
        } else {
            fragments.addSql(columnFullName, "::jsonb", "@>");
        }
        appendJson(fragments, value);
        if (not) {
            fragments.addSql(")");
        }
        return fragments;
    }

    @Override
    protected SqlFragments createContainedFragments(String columnFullName,
                                                   RDBColumnMetadata column,
                                                   Object value,
                                                   boolean not) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        if (not) {
            fragments.addSql("not", "(", columnFullName, "::jsonb", "<@");
        } else {
            fragments.addSql(columnFullName, "::jsonb", "<@");
        }
        appendJson(fragments, value);
        if (not) {
            fragments.addSql(")");
        }
        return fragments;
    }

    @Override
    protected JsonScalarExpression createScalarExpression(String columnFullName,
                                                         RDBColumnMetadata column,
                                                         JsonValueCondition condition,
                                                         boolean number) {
        List<String> segments = JsonPathUtils.segments(condition.getPath());
        String expression = "(" + columnFullName + "::jsonb #>> " + pathArraySql(segments) + ")";
        if (number) {
            expression = "cast(" + expression + " as numeric)";
        }
        return JsonScalarExpression.of(expression, segments.stream().map(Object.class::cast).toList());
    }

    private String pathArraySql(List<String> segments) {
        if (segments.isEmpty()) {
            return "array[]::text[]";
        }
        return "array[" + String.join(",", Collections.nCopies(segments.size(), "?")) + "]";
    }

    private void appendJson(PrepareSqlFragments fragments, Object value) {
        Object json = encodeJson(value);
        if (json instanceof NativeSql nativeSql) {
            fragments.addSql(nativeSql.getSql()).addParameter(nativeSql.getParameters());
        } else {
            fragments.addSql("?::jsonb").addParameter(json);
        }
    }
}
