package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.supports.json.*;

import java.util.Collections;

public class OracleJsonTermFragmentBuilder extends AbstractJsonTermFragmentBuilder {

    public static final OracleJsonTermFragmentBuilder exists =
        new OracleJsonTermFragmentBuilder(JsonTermType.exists, "json路径存在", Operation.exists, false);

    public static final OracleJsonTermFragmentBuilder notExists =
        new OracleJsonTermFragmentBuilder(JsonTermType.notExists, "json路径不存在", Operation.exists, true);

    public static final OracleJsonTermFragmentBuilder contains =
        new OracleJsonTermFragmentBuilder(JsonTermType.contains, "json包含", Operation.contains, false);

    public static final OracleJsonTermFragmentBuilder notContains =
        new OracleJsonTermFragmentBuilder(JsonTermType.notContains, "json不包含", Operation.contains, true);

    public static final OracleJsonTermFragmentBuilder value =
        new OracleJsonTermFragmentBuilder(JsonTermType.value, "json值查询", Operation.value, false);

    public OracleJsonTermFragmentBuilder(String termType, String name, Operation operation, boolean not) {
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
        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        if (not) {
            fragments.addSql("not");
        }
        return fragments.addSql("json_exists(", columnFullName, ",", "?", ")")
                        .addParameter(JsonPathUtils.normalize(path));
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
        String expression = number ? "json_value(" + columnFullName + ",? returning number)" : "json_value(" + columnFullName + ",?)";
        return JsonScalarExpression.of(expression, Collections.singletonList(JsonPathUtils.normalize(condition.getPath())));
    }
}
