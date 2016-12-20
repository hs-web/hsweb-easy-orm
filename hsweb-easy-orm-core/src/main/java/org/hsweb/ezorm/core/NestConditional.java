package org.hsweb.ezorm.core;

import org.hsweb.ezorm.core.param.TermType;
import org.hswebframwork.utils.StringUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;
import java.util.function.Function;

public interface NestConditional<T extends TermTypeConditionalSupport> extends TermTypeConditionalSupport {

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
        return accept(column, TermType.like, StringUtils.concat(value, "%"));
    }

    default NestConditional<T> $like(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat("%", value));
    }

    default NestConditional<T> $like$(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat("%", value, "%"));
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

    Accepter<NestConditional<T>> getAccepter();


    /**
     * @see Conditional#each(String, Collection, Function)
     */
    default NestConditional<T> each(String column, Collection list, Function<NestConditional<T>, SimpleAccepter<NestConditional<T>>> accepterGetter) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(this).accept(column, o));
        return this;
    }

    /**
     * @see Conditional#each(String, String, Collection, Function)
     */
    default NestConditional<T> each(String column, String termType, Collection list, Function<NestConditional<T>, Accepter<NestConditional<T>>> accepterGetter) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(this).accept(column, termType, o));
        return this;
    }

    /**
     * @see Conditional#each(String, Collection, Function)
     */
    default NestConditional<T> each(String column, Collection list, Function<NestConditional<T>, SimpleAccepter<NestConditional<T>>> accepterGetter, Function<Object, Object> valueMapper) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(this).accept(column, valueMapper.apply(o)));
        return this;
    }

    /**
     * @see Conditional#each(String, String, Collection, Function)
     */
    default NestConditional<T> each(String column, String termType, Collection list, Function<NestConditional<T>, Accepter<NestConditional<T>>> accepterGetter, Function<Object, Object> valueMapper) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(this).accept(column, termType, valueMapper.apply(o)));
        return this;
    }

    /**
     * @see Conditional#each(Map, Function)
     */
    default NestConditional<T> each(Map<String, Object> mapParam, Function<NestConditional<T>, SimpleAccepter<NestConditional<T>>> accepter) {
        if (null != mapParam)
            mapParam.forEach((k, v) -> accepter.apply(this).accept(k, v));
        return this;
    }

    /**
     * @see Conditional#each(Map, String, Function)
     */
    default NestConditional<T> each(Map<String, Object> mapParam, String termType, Function<NestConditional<T>, Accepter<NestConditional<T>>> accepter) {
        if (null != mapParam)
            mapParam.forEach((k, v) -> accepter.apply(this).accept(k, termType, v));
        return this;
    }

    /**
     * @see Conditional#when(boolean, Consumer)
     */
    default NestConditional<T> when(boolean condition, Consumer<NestConditional<T>> consumer) {
        if (condition) {
            consumer.accept(this);
        }
        return this;
    }

    /**
     * @see Conditional#when(BooleanSupplier, Consumer)
     */
    default NestConditional<T> when(BooleanSupplier condition, Consumer<NestConditional<T>> consumer) {
        return when(condition.getAsBoolean(), consumer);
    }

    /**
     * @see Conditional#when(boolean, String, Object, Function)
     */
    default NestConditional<T> when(boolean condition, String column, Object value, Function<NestConditional<T>, SimpleAccepter<NestConditional<T>>> accepter) {
        if (condition) {
            accepter.apply(this).accept(column, value);
        }
        return this;
    }

    /**
     * @see Conditional#when(String, Object, Function, Function)
     */
    default <V> NestConditional<T> when(String column, V value, Function<V, Boolean> condition, Function<NestConditional<T>, SimpleAccepter<NestConditional<T>>> accepter) {
        return when(condition.apply(value), column, value, accepter);
    }

    /**
     * @see Conditional#when(boolean, String, String, Object, Function)
     */
    default NestConditional<T> when(boolean condition, String column, String termType, Object value, Function<NestConditional<T>, Accepter<NestConditional<T>>> accepter) {
        if (condition) {
            accepter.apply(this).accept(column, termType, value);
        }
        return this;
    }

    /**
     * @see Conditional#when(String, String, Object, Function, Function)
     */
    default <V> NestConditional<T> when(String column, String termType, V value, Function<V, Boolean> condition, Function<NestConditional<T>, Accepter<NestConditional<T>>> accepter) {
        return when(condition.apply(value), column, termType, value, accepter);
    }

}
