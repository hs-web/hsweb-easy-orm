package org.hswebframework.ezorm.rdb.supports.postgres;

import com.google.common.collect.Lists;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.codec.JsonValueCodec;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.AbstractTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.utils.SqlUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class PostgresqlJsonbExistTermFragmentBuilder extends AbstractTermFragmentBuilder {

    public static final PostgresqlJsonbExistTermFragmentBuilder exist = new PostgresqlJsonbExistTermFragmentBuilder("exist", "jsonb包含");

    interface Options {
        String contains = "contains";
        String contained = "contained";
        String all = "all";
        String any = "any";
        String key = "key";
        String keys = "keys";
        String json = "json";
    }


    public PostgresqlJsonbExistTermFragmentBuilder(String termType, String name) {
        super(termType, name);
    }


    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {
        return createFragments(columnFullName, column, getOperation(term), term.getValue());
    }

    static SqlFragments createFragments(String columnFullName,
                                        RDBColumnMetadata column,
                                        PostgresqlJsonbTermFragmentBuilder.JsonbOperation operation,
                                        Object value) {
        return switch (operation) {
            case contains -> createObjectFragments(columnFullName, column, Operator.contains, value);
            case contained -> createObjectFragments(columnFullName, column, Operator.contained, value);
            case existsAll -> createArrayFragments(columnFullName, Operator.all, value);
            case existsAny -> createArrayFragments(columnFullName, Operator.any, value);
            case exists -> createBaseFragments(columnFullName, value);
        };
    }

    private static SqlFragments createObjectFragments(String columnFullName,
                                                      RDBColumnMetadata column,
                                                      String operator,
                                                      Object value) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        fragments.addSql(columnFullName, operator);
        return appendPrepareOrNativeValue(fragments, convertObjectValue(column, value));
    }

    private static SqlFragments createArrayFragments(String columnFullName, String operator, Object value) {
        //转换值
        List<Object> values = convertList(value);
        if (values.isEmpty()) {
            return EmptySqlFragments.INSTANCE;
        }
        return new BatchSqlFragments(4, 1)
            .addSql(operator, "(", columnFullName, ",")
            .addSql("array[")
            .add(SqlUtils.createQuestionMarks(values.size()))
            .addSql("])")
            .addParameter(values);
    }

    private static SqlFragments createBaseFragments(String columnFullName, Object value) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        fragments.addSql(Operator.base, "(", columnFullName, ",");
        appendPrepareOrNativeValue(fragments, value);
        return fragments.addSql(")");
    }

    private static <T extends AppendableSqlFragments> T appendPrepareOrNativeValue(T sql, Object value) {
        if (value instanceof NativeSql) {
            NativeSql nativeSql = ((NativeSql) value);
            sql.addSql(nativeSql.getSql())
               .addParameter(nativeSql.getParameters());
        } else {
            sql.add(SqlFragments.QUESTION_MARK)
               .addParameter(value);
        }
        return sql;
    }

    @SneakyThrows
    private static Object convertObjectValue(RDBColumnMetadata column, Object value) {
        if (value instanceof NativeSql) {
            return value;
        }
        if (column.getValueCodec() != null) {
            Object encoded = column.getValueCodec().encode(value);
            return encoded == null || encoded instanceof NativeSql ? encoded : NativeSql.of("?::jsonb", encoded);
        }
        Object obj;
        if (value == null) {
            return null;
        }
        if (value instanceof CharSequence) {
            obj = value.toString();
        } else {
            obj = JsonValueCodec.defaultMapper.writeValueAsString(value);
        }
        return NativeSql.of("?::jsonb", obj);
    }


    private static List<Object> convertList(Object value) {
        if (value == null) {
            return Collections.emptyList();
        }
        if (value instanceof String v) {
            value = v.split(",");
        }
        if (value instanceof Object[] v) {
            return Lists.newArrayList(v);
        }
        if (value instanceof Collection<?> v) {
            return new ArrayList<>(v);
        }
        return Collections.singletonList(value);
    }

    public static String getOperator(Term term) {
        return toOperator(getOperation(term));
    }

    private static PostgresqlJsonbTermFragmentBuilder.JsonbOperation getOperation(Term term) {
        List<String> options = term.getOptions();
        if (options.contains(Options.contains)) {
            return PostgresqlJsonbTermFragmentBuilder.JsonbOperation.contains;
        }
        if (options.contains(Options.contained)) {
            return PostgresqlJsonbTermFragmentBuilder.JsonbOperation.contained;
        }
        if (options.contains(Options.all)) {
            return PostgresqlJsonbTermFragmentBuilder.JsonbOperation.existsAll;
        }
        if (options.contains(Options.any) || options.contains(Options.keys)) {
            return PostgresqlJsonbTermFragmentBuilder.JsonbOperation.existsAny;
        }
        return PostgresqlJsonbTermFragmentBuilder.JsonbOperation.exists;
    }

    private static String toOperator(PostgresqlJsonbTermFragmentBuilder.JsonbOperation operation) {
        return switch (operation) {
            case contains -> Operator.contains;
            case contained -> Operator.contained;
            case existsAll -> Operator.all;
            case existsAny -> Operator.any;
            default -> Operator.base;
        };
    }

    private interface Operator {
        //用函数规避SimpleParameterList#checkAllParametersSet的检查
        String base = "jsonb_exists";
        String all = "jsonb_exists_all";
        String any = "jsonb_exists_any";
        String contains = "@>";
        String contained = "<@";

    }
}
