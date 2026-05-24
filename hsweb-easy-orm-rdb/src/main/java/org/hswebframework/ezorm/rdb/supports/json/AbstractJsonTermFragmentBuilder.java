package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.AbstractTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.utils.SqlUtils;

import java.util.*;
import java.util.stream.Collectors;

public abstract class AbstractJsonTermFragmentBuilder extends AbstractTermFragmentBuilder {

    public enum Operation {
        exists,
        contains,
        contained,
        value
    }

    private final Operation operation;

    private final boolean not;

    protected AbstractJsonTermFragmentBuilder(String termType, String name, Operation operation, boolean not) {
        super(termType, name);
        this.operation = operation;
        this.not = not;
    }

    public static void addJsonFeatures(RDBColumnMetadata column, AbstractJsonTermFragmentBuilder... builders) {
        if (!JsonDataType.isJson(column)) {
            return;
        }
        for (AbstractJsonTermFragmentBuilder builder : builders) {
            column.addFeature(builder);
        }
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {
        return switch (operation) {
            case exists -> createExistsFragments(columnFullName, column, term.getValue(), not);
            case contains -> createContainsFragments(columnFullName, column, term.getValue(), not);
            case contained -> createContainedFragments(columnFullName, column, term.getValue(), not);
            case value -> createValueFragments(columnFullName, column, JsonValueCondition.of(term), not);
        };
    }

    protected abstract SqlFragments createExistsFragments(String columnFullName,
                                                          RDBColumnMetadata column,
                                                          Object path,
                                                          boolean not);

    protected abstract SqlFragments createContainsFragments(String columnFullName,
                                                            RDBColumnMetadata column,
                                                            Object value,
                                                            boolean not);

    protected SqlFragments createContainedFragments(String columnFullName,
                                                    RDBColumnMetadata column,
                                                    Object value,
                                                    boolean not) {
        throw new UnsupportedOperationException(getName() + " does not support json contained query");
    }

    protected abstract JsonScalarExpression createScalarExpression(String columnFullName,
                                                                   RDBColumnMetadata column,
                                                                   JsonValueCondition condition,
                                                                   boolean number);

    protected SqlFragments createValueFragments(String columnFullName,
                                                RDBColumnMetadata column,
                                                JsonValueCondition condition,
                                                boolean not) {
        String termType = condition.getTermType();
        Object value = condition.getValue();
        boolean number = isNumberExpression(termType, value);
        JsonScalarExpression expression = createScalarExpression(columnFullName, column, condition, number);
        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        fragments.addSql(expression.getSql())
                 .addParameter(expression.getParameters());

        if (TermType.isnull.equals(termType)) {
            fragments.addSql(not ? "is not null" : "is null");
            return fragments;
        }
        if (TermType.notnull.equals(termType)) {
            fragments.addSql(not ? "is null" : "is not null");
            return fragments;
        }

        if (TermType.in.equals(termType) || TermType.nin.equals(termType)) {
            List<Object> values = convertValueList(value, number);
            if (values.isEmpty()) {
                return EmptySqlFragments.INSTANCE;
            }
            boolean negative = TermType.nin.equals(termType) ^ not;
            fragments.addSql(negative ? "not in(" : "in(")
                     .addFragments(SqlUtils.createQuestionMarks(values.size()))
                     .addSql(")")
                     .addParameter(values);
            return fragments;
        }

        String operator = toSqlOperator(termType);
        if (operator == null) {
            throw new UnsupportedOperationException("Unsupported json value term type: " + termType);
        }
        if (not) {
            operator = reverse(operator);
        }
        fragments.addSql(operator);
        appendPrepareOrNative(fragments, convertCompareValue(value, number));
        return fragments;
    }

    protected SqlFragments createContainsByValueFragments(String columnFullName,
                                                          RDBColumnMetadata column,
                                                          Object value,
                                                          boolean not) {
        Map<String, Object> values = flatten(value);
        if (values.isEmpty()) {
            return EmptySqlFragments.INSTANCE;
        }
        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        if (not) {
            fragments.addSql("not", "(");
        } else {
            fragments.addSql("(");
        }
        int index = 0;
        for (Map.Entry<String, Object> entry : values.entrySet()) {
            if (index++ > 0) {
                fragments.addSql("and");
            }
            JsonValueCondition condition = JsonValueCondition.of(entry.getKey(), entry.getValue());
            SqlFragments part = createValueFragments(columnFullName, column, condition, false);
            fragments.addSql(part.getSql()).addParameter(part.getParameters());
        }
        fragments.addSql(")");
        return fragments;
    }

    protected Object encodeJson(Object value) {
        if (value instanceof NativeSql) {
            return value;
        }
        return JsonCodecSupport.toJsonSilently(value);
    }

    protected String nullableNot(boolean not) {
        return not ? "not" : "";
    }

    private boolean isNumberExpression(String termType, Object value) {
        if (TermType.like.equals(termType) || TermType.nlike.equals(termType)) {
            return false;
        }
        if (value instanceof Number) {
            return true;
        }
        if (value instanceof Collection<?>) {
            Collection<?> collection = ((Collection<?>) value);
            return !collection.isEmpty() && collection.stream().allMatch(Number.class::isInstance);
        }
        if (value instanceof Object[]) {
            Object[] arr = ((Object[]) value);
            return arr.length > 0 && Arrays.stream(arr).allMatch(Number.class::isInstance);
        }
        return TermType.gt.equals(termType) ||
            TermType.gte.equals(termType) ||
            TermType.lt.equals(termType) ||
            TermType.lte.equals(termType);
    }

    private Object convertCompareValue(Object value, boolean number) {
        if (value instanceof NativeSql || value == null || number) {
            return value;
        }
        return String.valueOf(value);
    }

    @SuppressWarnings("all")
    private List<Object> convertValueList(Object value, boolean number) {
        if (value == null) {
            return Collections.emptyList();
        }
        if (value instanceof Collection<?>) {
            return ((Collection<?>) value)
                .stream()
                .map(val -> convertCompareValue(val, number))
                .collect(Collectors.toList());
        }
        if (value instanceof Object[]) {
            return Arrays.stream(((Object[]) value))
                         .map(val -> convertCompareValue(val, number))
                         .collect(Collectors.toList());
        }
        if (value instanceof String && !number) {
            return Arrays.asList(((String) value).split(","));
        }
        return Collections.singletonList(convertCompareValue(value, number));
    }

    private String toSqlOperator(String termType) {
        return switch (termType) {
            case TermType.eq, "is" -> "=";
            case TermType.not -> "!=";
            case TermType.gt -> ">";
            case TermType.gte -> ">=";
            case TermType.lt -> "<";
            case TermType.lte -> "<=";
            case TermType.like -> "like";
            case TermType.nlike -> "not like";
            default -> null;
        };
    }

    private String reverse(String operator) {
        return switch (operator) {
            case "=" -> "!=";
            case "!=" -> "=";
            case ">" -> "<=";
            case ">=" -> "<";
            case "<" -> ">=";
            case "<=" -> ">";
            case "like" -> "not like";
            case "not like" -> "like";
            default -> "not " + operator;
        };
    }

    @SuppressWarnings("all")
    private Map<String, Object> flatten(Object value) {
        if (!(value instanceof Map)) {
            return Collections.emptyMap();
        }
        Map<String, Object> values = new LinkedHashMap<>();
        flatten("", ((Map<?, ?>) value), values);
        return values;
    }

    private void flatten(String prefix, Map<?, ?> source, Map<String, Object> target) {
        for (Map.Entry<?, ?> entry : source.entrySet()) {
            if (entry.getKey() == null) {
                continue;
            }
            String path = prefix.isEmpty() ? String.valueOf(entry.getKey()) : prefix + "." + entry.getKey();
            Object value = entry.getValue();
            if (value instanceof Map<?, ?> map) {
                flatten(path, map, target);
            } else {
                target.put(path, value);
            }
        }
    }
}
