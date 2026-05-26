package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.supports.json.*;

import java.util.Collections;

public class MysqlJsonTermFragmentBuilder extends AbstractJsonTermFragmentBuilder {

    public static final MysqlJsonTermFragmentBuilder exists =
        new MysqlJsonTermFragmentBuilder(JsonTermType.exists, "json路径存在", Operation.exists, false);

    public static final MysqlJsonTermFragmentBuilder notExists =
        new MysqlJsonTermFragmentBuilder(JsonTermType.notExists, "json路径不存在", Operation.exists, true);

    public static final MysqlJsonTermFragmentBuilder contains =
        new MysqlJsonTermFragmentBuilder(JsonTermType.contains, "json包含", Operation.contains, false);

    public static final MysqlJsonTermFragmentBuilder notContains =
        new MysqlJsonTermFragmentBuilder(JsonTermType.notContains, "json不包含", Operation.contains, true);

    public static final MysqlJsonTermFragmentBuilder contained =
        new MysqlJsonTermFragmentBuilder(JsonTermType.contained, "json被包含", Operation.contained, false);

    public static final MysqlJsonTermFragmentBuilder notContained =
        new MysqlJsonTermFragmentBuilder(JsonTermType.notContained, "json不被包含", Operation.contained, true);

    public static final MysqlJsonTermFragmentBuilder value =
        new MysqlJsonTermFragmentBuilder(JsonTermType.value, "json值查询", Operation.value, false);

    public MysqlJsonTermFragmentBuilder(String termType, String name, Operation operation, boolean not) {
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
        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        if (not) {
            fragments.addSql("not");
        }
        return fragments
            .addSql("json_contains_path(", columnFullName, ",'one',", "?", ")")
            .addParameter(JsonPathUtils.normalize(path));
    }

    @Override
    protected SqlFragments createContainsFragments(String columnFullName,
                                                   RDBColumnMetadata column,
                                                   Object value,
                                                   boolean not) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        if (not) {
            fragments.addSql("not");
        }
        fragments.addSql("json_contains(", columnFullName, ",");
        appendJson(fragments, value);
        return fragments.addSql(")");
    }

    @Override
    protected SqlFragments createContainedFragments(String columnFullName,
                                                    RDBColumnMetadata column,
                                                    Object value,
                                                    boolean not) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        if (not) {
            fragments.addSql("not");
        }
        fragments.addSql("json_contains(");
        appendJson(fragments, value);
        return fragments.addSql(",", columnFullName, ")");
    }

    @Override
    protected JsonScalarExpression createScalarExpression(String columnFullName,
                                                         RDBColumnMetadata column,
                                                         JsonValueCondition condition,
                                                         boolean number) {
        String expression = "json_unquote(json_extract(" + columnFullName + ",?))";
        if (number) {
            expression = "cast(" + expression + " as decimal(65,30))";
        }
        return JsonScalarExpression.of(expression, Collections.singletonList(JsonPathUtils.normalize(condition.getPath())));
    }

    private void appendJson(PrepareSqlFragments fragments, Object value) {
        Object json = encodeJson(value);
        if (json instanceof NativeSql nativeSql) {
            fragments.addSql(nativeSql.getSql()).addParameter(nativeSql.getParameters());
        } else {
            fragments.addSql("?").addParameter(json);
        }
    }
}
