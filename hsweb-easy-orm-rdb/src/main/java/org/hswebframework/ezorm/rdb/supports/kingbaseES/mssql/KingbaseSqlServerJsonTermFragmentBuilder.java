package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.supports.json.AbstractJsonTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.json.JsonPathUtils;
import org.hswebframework.ezorm.rdb.supports.json.JsonScalarExpression;
import org.hswebframework.ezorm.rdb.supports.json.JsonTermType;
import org.hswebframework.ezorm.rdb.supports.json.JsonValueCondition;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerJsonTermFragmentBuilder;

import java.util.Collections;
import java.util.List;

/** Retains SQL Server JSON predicates, substituting Kingbase's available extraction functions. */
public class KingbaseSqlServerJsonTermFragmentBuilder extends SqlServerJsonTermFragmentBuilder {

    public static final KingbaseSqlServerJsonTermFragmentBuilder exists =
        new KingbaseSqlServerJsonTermFragmentBuilder(JsonTermType.exists, "json路径存在", Operation.exists, false);
    public static final KingbaseSqlServerJsonTermFragmentBuilder notExists =
        new KingbaseSqlServerJsonTermFragmentBuilder(JsonTermType.notExists, "json路径不存在", Operation.exists, true);
    public static final KingbaseSqlServerJsonTermFragmentBuilder contains =
        new KingbaseSqlServerJsonTermFragmentBuilder(JsonTermType.contains, "json包含", Operation.contains, false);
    public static final KingbaseSqlServerJsonTermFragmentBuilder notContains =
        new KingbaseSqlServerJsonTermFragmentBuilder(JsonTermType.notContains, "json不包含", Operation.contains, true);
    public static final KingbaseSqlServerJsonTermFragmentBuilder value =
        new KingbaseSqlServerJsonTermFragmentBuilder(JsonTermType.value, "json值查询", Operation.value, false);

    public KingbaseSqlServerJsonTermFragmentBuilder(String type, String name, Operation operation, boolean not) {
        super(type, name, operation, not);
    }

    public static void addJsonFeatures(RDBColumnMetadata column) {
        AbstractJsonTermFragmentBuilder.addJsonFeatures(column, exists, notExists, contains, notContains, value);
    }

    @Override
    protected SqlFragments createExistsFragments(String name, RDBColumnMetadata column, Object path, boolean not) {
        JsonScalarExpression expression = extract(name, path, false);
        return PrepareSqlFragments.of()
            .addSql("(", expression.getSql(), not ? "is null or" : "is not null and",
                expression.getSql(), not ? "= cast('null' as jsonb)" : "<> cast('null' as jsonb)", ")")
            .addParameter(expression.getParameters()).addParameter(expression.getParameters());
    }

    @Override
    protected JsonScalarExpression createScalarExpression(String name, RDBColumnMetadata column,
                                                          JsonValueCondition condition, boolean number) {
        JsonScalarExpression expression = extract(name, condition.getPath(), true);
        return number ? JsonScalarExpression.of("cast(" + expression.getSql() + " as decimal(38,10))", expression.getParameters())
            : expression;
    }

    private JsonScalarExpression extract(String name, Object path, boolean text) {
        List<String> segments = JsonPathUtils.segments(path);
        String arguments = String.join("", Collections.nCopies(segments.size(), ", cast(? as pg_catalog.text)"));
        return JsonScalarExpression.of((text ? "jsonb_extract_path_text" : "jsonb_extract_path")
            + "(cast(" + name + " as jsonb)" + arguments + ")", segments.stream().map(s -> (Object) s).toList());
    }
}
