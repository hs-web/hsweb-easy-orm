package org.hsweb.ezorm.core;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.core.param.TermType;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.function.Function;

public interface NestConditional<T extends TermTypeConditionalSupport> extends TermTypeConditionalSupport {

    T end();

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
     * 遍历一个集合，进行条件追加
     * 例如:<br>
     * query.or().each("areaId",[1,2,3],(query)->query::$like$)<br>
     * 将追加sql<br>
     * areaId like '%1%' or areaId like '%2%' or areaId like '%3%'
     *
     * @param column         要追加到的列名
     * @param list           集合
     * @param accepterGetter 追加方式函数
     * @return {@link T}
     * @see Function
     * @see Conditional
     * @see org.hsweb.ezorm.core.TermTypeConditionalSupport.SimpleAccepter
     */
    default NestConditional<T> each(String column, Collection list, Function<NestConditional<T>, SimpleAccepter<NestConditional<T>>> accepterGetter) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(this).accept(column, o));
        return this;
    }

    /**
     * 参照 {@link Conditional#each(String, Collection, Function)}
     * 提供了一个valueMapper进行值转换如:
     * <br>
     * query.or().each("areaId",[1,2,3],(query)->query::$like$,(value)->","+value+",")<br>
     * 将追加sql<br>
     * areaId like '%,1,%' or areaId like '%,2,%' or areaId like '%,3,%'
     *
     * @param column         要追加到的列名
     * @param list           集合
     * @param accepterGetter 追加方式函数
     * @param valueMapper    值转换函数 {@link Function}
     * @return {@link T}
     * @see Function
     * @see Conditional
     * @see org.hsweb.ezorm.core.TermTypeConditionalSupport.SimpleAccepter
     */
    default NestConditional<T> each(String column, Collection list, Function<NestConditional<T>, SimpleAccepter<NestConditional<T>>> accepterGetter, Function<Object, Object> valueMapper) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(this).accept(column, valueMapper.apply(o)));
        return this;
    }

    /**
     * 遍历一个Map,进行条件追加
     * 例如:<br>
     * query.or().each({name:"admin"},(query)->query::$like$)<br>
     * 将追加sql<br>
     * name like '%admin%'
     *
     * @param mapParam map参数
     * @param accepter 追加方式函数
     * @return {@link T}
     * @see Function
     * @see Conditional
     * @see org.hsweb.ezorm.core.TermTypeConditionalSupport.SimpleAccepter
     */
    default NestConditional<T> each(Map<String, Object> mapParam, Function<NestConditional<T>, SimpleAccepter<NestConditional<T>>> accepter) {
        if (null != mapParam)
            mapParam.forEach((k, v) -> accepter.apply(this).accept(k, v));
        return this;
    }

}
