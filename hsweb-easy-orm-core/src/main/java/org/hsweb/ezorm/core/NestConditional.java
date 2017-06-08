package org.hsweb.ezorm.core;

import org.hsweb.ezorm.core.param.TermType;

import java.util.Arrays;
import java.util.Collection;

public interface NestConditional<T extends TermTypeConditionalSupport> extends LogicalOperation<NestConditional<T>>, TermTypeConditionalSupport {

    T end();

    default NestConditional<T> is(String column, Object value) {
        return accept(column, TermType.eq, value);
    }

    NestConditional<NestConditional<T>> nest();

    NestConditional<NestConditional<T>> nest(String column, Object value);

    NestConditional<NestConditional<T>> orNest();

    NestConditional<NestConditional<T>> orNest(String column, Object value);

    NestConditional<T> and();

    NestConditional<T> or();

    NestConditional<T> and(String column, String termType, Object value);

    NestConditional<T> or(String column, String termType, Object value);

    default NestConditional<T> and(String column, Object value) {
        and();
        return and(column, TermType.eq, value);
    }

    default NestConditional<T> or(String column, Object value) {
        or();
        return or(column, TermType.eq, value);
    }

    default NestConditional<T> like(String column, Object value) {
        return accept(column, TermType.like, value);
    }

    default NestConditional<T> like$(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, String.valueOf(value).concat("%"));
    }

    default NestConditional<T> $like(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, "%".concat(String.valueOf(value)));
    }

    default NestConditional<T> $like$(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, "%".concat(String.valueOf(value)).concat("%"));
    }

    /**
     * 直接拼接sql,参数支持预编译
     * 例如
     * <ul>
     * <li>query.sql("name=?","admin")</li>
     * <li>query.sql("name=#{name}",{name:"admin"})</li>
     * <li>query.sql("name=#{[0]}",["admin"])</li>
     * </ul>
     *
     * @param sql    sql字符串
     * @param params 参数
     * @return {@link T}
     */
    NestConditional<T> sql(String sql, Object... params);

    default NestConditional<T> notLike(String column, Object value) {
        return accept(column, TermType.nlike, value);
    }

    default NestConditional<T> gt(String column, Object value) {
        return accept(column, TermType.gt, value);
    }

    default NestConditional<T> lt(String column, Object value) {
        return accept(column, TermType.lt, value);
    }

    default NestConditional<T> gte(String column, Object value) {
        return accept(column, TermType.gte, value);
    }

    default NestConditional<T> lte(String column, Object value) {
        return accept(column, TermType.lte, value);
    }

    default NestConditional<T> in(String column, Object... values) {
        return accept(column, TermType.in, values);
    }

    default NestConditional<T> in(String column, Object value) {
        return accept(column, TermType.in, value);
    }

    default NestConditional<T> in(String column, Collection values) {
        return accept(column, TermType.in, values);
    }

    default NestConditional<T> notIn(String column, Object value) {
        return accept(column, TermType.nin, value);
    }

    default NestConditional<T> isEmpty(String column) {
        return accept(column, TermType.empty, 1);
    }

    default NestConditional<T> notEmpty(String column) {
        return accept(column, TermType.nempty, 1);
    }

    default NestConditional<T> isNull(String column) {
        return accept(column, TermType.isnull, 1);
    }

    default NestConditional<T> notNull(String column) {
        return accept(column, TermType.notnull, 1);
    }

    default NestConditional<T> not(String column, Object value) {
        return accept(column, TermType.not, value);
    }

    default NestConditional<T> between(String column, Object between, Object and) {
        return accept(column, TermType.btw, Arrays.asList(between, and));
    }

    default NestConditional<T> notBetween(String column, Object between, Object and) {
        return accept(column, TermType.nbtw, Arrays.asList(between, and));
    }

    default NestConditional<T> accept(String column, String termType, Object value) {
        return getAccepter().accept(column, termType, value);
    }

    Accepter<NestConditional<T>, Object> getAccepter();

}
