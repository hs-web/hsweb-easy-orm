package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.supports.json.*;

import java.util.Collections;

public class SqlServerJsonTermFragmentBuilder extends AbstractJsonTermFragmentBuilder {

    public static final SqlServerJsonTermFragmentBuilder exists =
        new SqlServerJsonTermFragmentBuilder(JsonTermType.exists, "json路径存在", Operation.exists, false);

    public static final SqlServerJsonTermFragmentBuilder notExists =
        new SqlServerJsonTermFragmentBuilder(JsonTermType.notExists, "json路径不存在", Operation.exists, true);

    public static final SqlServerJsonTermFragmentBuilder contains =
        new SqlServerJsonTermFragmentBuilder(JsonTermType.contains, "json包含", Operation.contains, false);

    public static final SqlServerJsonTermFragmentBuilder notContains =
        new SqlServerJsonTermFragmentBuilder(JsonTermType.notContains, "json不包含", Operation.contains, true);

    public static final SqlServerJsonTermFragmentBuilder value =
        new SqlServerJsonTermFragmentBuilder(JsonTermType.value, "json值查询", Operation.value, false);

    public SqlServerJsonTermFragmentBuilder(String termType, String name, Operation operation, boolean not) {
        super(termType, name, operation, not);
    }

    public static void addJsonFeatures(RDBColumnMetadata column) {
        AbstractJsonTermFragmentBuilder.addJsonFeatures(column, exists, notExists, contains, notContains, value);
    }

    @Override
    protected SqlFragments createExistsFragments(String columnFullName,
                                                 RDBColumnMetadata column,
                                                 Object path,
                                                 boolean not) {
        String normalizedPath = JsonPathUtils.normalize(path);
        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        if (not) {
            fragments.addSql("not");
        }
        return fragments.addSql("(", "json_value(", columnFullName, ",?", ")", "is not null",
                                "or", "json_query(", columnFullName, ",?", ")", "is not null", ")")
                        .addParameter(normalizedPath, normalizedPath);
    }

    @Override
    protected SqlFragments createContainsFragments(String columnFullName,
                                                   RDBColumnMetadata column,
                                                   Object value,
                                                   boolean not) {
        return createContainsByValueFragments(columnFullName, column, value, not);
    }

    @Override
    protected JsonScalarExpression createScalarExpression(String columnFullName,
                                                         RDBColumnMetadata column,
                                                         JsonValueCondition condition,
                                                         boolean number) {
        String expression = "json_value(" + columnFullName + ",?)";
        if (number) {
            expression = "cast(" + expression + " as decimal(38,10))";
        }
        return JsonScalarExpression.of(expression, Collections.singletonList(JsonPathUtils.normalize(condition.getPath())));
    }
}
