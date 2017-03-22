package org.hsweb.ezorm.core;

import org.hsweb.ezorm.core.param.TermType;

import java.util.Arrays;
import java.util.Collection;

public interface NestConditionalFromBean<T extends TermTypeConditionalFromBeanSupport> extends LogicalOperation<NestConditionalFromBean<T>>, TermTypeConditionalFromBeanSupport {

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
        return accept(column, TermType.like, String.valueOf(value).concat("%"));
    }

    default NestConditionalFromBean<T> $like(String column) {
        Object value = getValue(column);
        if (value == null)
            return like(column);
        return accept(column, TermType.like, "%".concat(String.valueOf(value)));
    }

    default NestConditionalFromBean<T> $like$(String column) {
        Object value = getValue(column);
        if (value == null)
            return like(column);
        return accept(column, TermType.like, "%".concat(String.valueOf(value)).concat("%"));
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

    TermTypeConditionalSupport.Accepter<NestConditionalFromBean<T>, Object> getAccepter();


}
