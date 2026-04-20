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
    }


    public PostgresqlJsonbExistTermFragmentBuilder(String termType, String name) {
        super(termType, name);
    }


    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {
        String operator = getOperator(term);
        if (Operator.needObject(operator)) {
            PrepareSqlFragments fragments = PrepareSqlFragments.of();
            fragments.addSql(columnFullName, operator);
            Object value = term.getValue();
            return appendPrepareOrNative(fragments, convertObjectValue(column, value));
        }
        if (!Operator.base.equals(operator)) {
            //转换值
            List<Object> values = convertList(term.getValue());
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

        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        fragments.addSql(Operator.base, "(", columnFullName, ",");
        appendPrepareOrNative(fragments, term.getValue());
        return fragments.addSql(")");
    }

    @SneakyThrows
    private Object convertObjectValue(RDBColumnMetadata column, Object value) {
        if (value instanceof NativeSql) {
            return value;
        }
        if (column.getValueCodec() != null) {
            return column.getValueCodec().encode(value);
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


    private List<Object> convertList(Object value) {
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
        List<String> options = term.getOptions();
        if (options.contains(Options.contains)) {
            return Operator.contains;
        }
        if (options.contains(Options.contained)) {
            return Operator.contained;
        }
        if (options.contains(Options.all)) {
            return Operator.all;
        }
        if (options.contains(Options.any)) {
            return Operator.any;
        }
        return Operator.base;
    }

    private interface Operator {
        //用函数规避SimpleParameterList#checkAllParametersSet的检查
        String base = "jsonb_exists";
        String all = "jsonb_exists_all";
        String any = "jsonb_exists_any";
        String contains = "@>";
        String contained = "<@";

        static boolean needObject(String operator) {
            return contains.equals(operator) || contained.equals(operator);
        }
    }
}
