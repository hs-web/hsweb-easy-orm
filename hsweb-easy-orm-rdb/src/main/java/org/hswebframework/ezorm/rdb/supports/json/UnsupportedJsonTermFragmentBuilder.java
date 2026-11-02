package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

public class UnsupportedJsonTermFragmentBuilder extends AbstractJsonTermFragmentBuilder {

    public static final UnsupportedJsonTermFragmentBuilder exists =
        new UnsupportedJsonTermFragmentBuilder(JsonTermType.exists, Operation.exists, false);

    public static final UnsupportedJsonTermFragmentBuilder notExists =
        new UnsupportedJsonTermFragmentBuilder(JsonTermType.notExists, Operation.exists, true);

    public static final UnsupportedJsonTermFragmentBuilder contains =
        new UnsupportedJsonTermFragmentBuilder(JsonTermType.contains, Operation.contains, false);

    public static final UnsupportedJsonTermFragmentBuilder notContains =
        new UnsupportedJsonTermFragmentBuilder(JsonTermType.notContains, Operation.contains, true);

    public static final UnsupportedJsonTermFragmentBuilder contained =
        new UnsupportedJsonTermFragmentBuilder(JsonTermType.contained, Operation.contained, false);

    public static final UnsupportedJsonTermFragmentBuilder notContained =
        new UnsupportedJsonTermFragmentBuilder(JsonTermType.notContained, Operation.contained, true);

    public static final UnsupportedJsonTermFragmentBuilder value =
        new UnsupportedJsonTermFragmentBuilder(JsonTermType.value, Operation.value, false);

    public UnsupportedJsonTermFragmentBuilder(String termType, Operation operation, boolean not) {
        super(termType, "不支持的JSON查询", operation, not);
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
    protected SqlFragments createExistsFragments(String columnFullName, RDBColumnMetadata column, Object path, boolean not) {
        throw unsupported(column);
    }

    @Override
    protected SqlFragments createContainsFragments(String columnFullName, RDBColumnMetadata column, Object value, boolean not) {
        throw unsupported(column);
    }

    @Override
    protected JsonScalarExpression createScalarExpression(String columnFullName,
                                                         RDBColumnMetadata column,
                                                         JsonValueCondition condition,
                                                         boolean number) {
        throw unsupported(column);
    }

    private UnsupportedOperationException unsupported(RDBColumnMetadata column) {
        return new UnsupportedOperationException(
            "Dialect " + column.getDialect().getName() + " does not support native json query"
        );
    }
}
