package org.hsweb.ezorm.core;

import org.hsweb.ezorm.core.param.TermType;
import org.hswebframwork.utils.StringUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;
import java.util.function.Function;

public interface NestConditionalFromBean<T extends TermTypeConditionalFromBeanSupport> extends TermTypeConditionalFromBeanSupport {

    T end();

    NestConditionalFromBean<NestConditionalFromBean<T>> nest();

    NestConditionalFromBean<NestConditionalFromBean<T>> nest(String column);

    NestConditionalFromBean<NestConditionalFromBean<T>> orNest();

    NestConditionalFromBean<NestConditionalFromBean<T>> orNest(String column);

    NestConditionalFromBean<T> and();

    NestConditionalFromBean<T> or();

    NestConditionalFromBean<T> and(String column, String termType);

    NestConditionalFromBean<T> or(String column, String termType);

    default NestConditionalFromBean<T> and(String column) {
        and();
        return and(column, TermType.eq);
    }

    default NestConditionalFromBean<T> or(String column) {
        or();
        return or(column, TermType.eq);
    }

    default NestConditionalFromBean<T> like(String column) {
        return accept(column, TermType.like);
    }

    NestConditionalFromBean<T> sql(String sql, Object... params);

    default NestConditionalFromBean<T> like$(String column) {
        Object value = getValue(column);
        if (value == null)
            return like(column);
        return accept(column, TermType.like, StringUtils.concat(value, "%"));
    }

    default NestConditionalFromBean<T> $like(String column) {
        Object value = getValue(column);
        if (value == null)
            return like(column);
        return accept(column, TermType.like, StringUtils.concat("%", value));
    }

    default NestConditionalFromBean<T> $like$(String column) {
        Object value = getValue(column);
        if (value == null)
            return like(column);
        return accept(column, TermType.like, StringUtils.concat("%", value, "%"));
    }

    default NestConditionalFromBean<T> notLike(String column) {
        return accept(column, TermType.nlike);
    }

    default NestConditionalFromBean<T> gt(String column) {
        return accept(column, TermType.gt);
    }

    default NestConditionalFromBean<T> lt(String column) {
        return accept(column, TermType.lt);
    }

    default NestConditionalFromBean<T> gte(String column) {
        return accept(column, TermType.gte);
    }

    default NestConditionalFromBean<T> lte(String column) {
        return accept(column, TermType.lte);
    }

    default NestConditionalFromBean<T> in(String column, Object... values) {
        return accept(column, TermType.in, values);
    }

    default NestConditionalFromBean<T> in(String column) {
        return accept(column, TermType.in);
    }

    default NestConditionalFromBean<T> in(String column, Collection values) {
        return accept(column, TermType.in, values);
    }

    default NestConditionalFromBean<T> notIn(String column) {
        return accept(column, TermType.nin);
    }

    default NestConditionalFromBean<T> isEmpty(String column) {
        return accept(column, TermType.empty, 1);
    }

    default NestConditionalFromBean<T> notEmpty(String column) {
        return accept(column, TermType.nempty, 1);
    }

    default NestConditionalFromBean<T> isNull(String column) {
        return accept(column, TermType.isnull, 1);
    }

    default NestConditionalFromBean<T> notNull(String column) {
        return accept(column, TermType.notnull, 1);
    }

    default NestConditionalFromBean<T> not(String column) {
        return accept(column, TermType.not);
    }

    default NestConditionalFromBean<T> between(String column, Object between, Object and) {
        return accept(column, TermType.btw, Arrays.asList(between, and));
    }

    default NestConditionalFromBean<T> notBetween(String column, Object between, Object and) {
        return accept(column, TermType.nbtw, Arrays.asList(between, and));
    }

    default NestConditionalFromBean<T> accept(String column, String termType, Object value) {
        return getAccepter().accept(column, termType, value);
    }

    default NestConditionalFromBean<T> accept(String column, String termType) {
        return getAccepter().accept(column, termType, getValue(column));
    }

    TermTypeConditionalSupport.Accepter<NestConditionalFromBean<T>> getAccepter();

    default NestConditionalFromBean<T> each(String column, Collection list, Function<NestConditionalFromBean<T>, TermTypeConditionalSupport.SimpleAccepter<NestConditionalFromBean<T>>> accepter) {
        list.forEach(o -> accepter.apply(this).accept(column, o));
        return this;
    }

    default NestConditionalFromBean<T> each(String column, Collection list, Function<NestConditionalFromBean<T>, TermTypeConditionalSupport.SimpleAccepter<NestConditionalFromBean<T>>> accepter, Function<Object, Object> valueMapper) {
        list.forEach(o -> accepter.apply(this).accept(column, valueMapper.apply(o)));
        return this;
    }

    default NestConditionalFromBean<T> each(Map<String, Object> mapParam, Function<NestConditionalFromBean<T>, TermTypeConditionalSupport.SimpleAccepter<NestConditionalFromBean<T>>> accepter) {
        mapParam.forEach((k, v) -> accepter.apply(this).accept(k, v));
        return this;
    }

    default NestConditionalFromBean<T> each(String column, String termType, Collection list, Function<NestConditionalFromBean<T>, TermTypeConditionalSupport.Accepter<NestConditionalFromBean<T>>> accepter) {
        list.forEach(o -> accepter.apply(this).accept(column, termType, o));
        return this;
    }

    default NestConditionalFromBean<T> each(String column, String termType, Collection list, Function<NestConditionalFromBean<T>, TermTypeConditionalSupport.Accepter<NestConditionalFromBean<T>>> accepter, Function<Object, Object> valueMapper) {
        list.forEach(o -> accepter.apply(this).accept(column, termType, valueMapper.apply(o)));
        return this;
    }

    default NestConditionalFromBean<T> each(Map<String, Object> mapParam, String termType, Function<NestConditionalFromBean<T>, TermTypeConditionalSupport.Accepter<NestConditionalFromBean<T>>> accepter) {
        mapParam.forEach((k, v) -> accepter.apply(this).accept(k, termType, v));
        return this;
    }


    default NestConditionalFromBean<T> when(boolean condition, Consumer<NestConditionalFromBean<T>> consumer) {
        if (condition) {
            consumer.accept(this);
        }
        return this;
    }

    default NestConditionalFromBean<T> when(BooleanSupplier condition, Consumer<NestConditionalFromBean<T>> consumer) {
        return when(condition.getAsBoolean(), consumer);
    }

    default NestConditionalFromBean<T> when(boolean condition, String column, Function<NestConditionalFromBean<T>, TermTypeConditionalSupport.SimpleAccepter<NestConditionalFromBean<T>>> accepter) {
        if (condition) {
            accepter.apply(this).accept(column, getValue(column));
        }
        return this;
    }

    default NestConditionalFromBean<T> when(String column, Function<Object, Boolean> condition, Function<NestConditionalFromBean<T>, TermTypeConditionalSupport.SimpleAccepter<NestConditionalFromBean<T>>> accepter) {
        Object value = getValue(column);
        return when(condition.apply(value), column, accepter);
    }

    default NestConditionalFromBean<T> when(boolean condition, String column, String termType, Function<NestConditionalFromBean<T>, TermTypeConditionalSupport.Accepter<NestConditionalFromBean<T>>> accepter) {
        if (condition) {
            accepter.apply(this).accept(column, termType, getValue(column));
        }
        return this;
    }

    default NestConditionalFromBean<T> when(String column, String termType, Function<Object, Boolean> condition, Function<NestConditionalFromBean<T>, TermTypeConditionalSupport.Accepter<NestConditionalFromBean<T>>> accepter) {
        Object value = getValue(column);
        return when(condition.apply(value), column, termType, accepter);
    }

}
