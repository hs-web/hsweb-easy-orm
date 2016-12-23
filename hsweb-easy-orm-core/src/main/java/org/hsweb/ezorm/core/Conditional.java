/*
 * Copyright 2016 http://github.com/hs-web
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.hsweb.ezorm.core;

import org.hsweb.ezorm.core.param.TermType;
import org.hswebframwork.utils.StringUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;
import java.util.function.Function;

public interface Conditional<T extends Conditional> extends TermTypeConditionalSupport {
    NestConditional<T> nest();

    NestConditional<T> nest(String column, Object value);

    NestConditional<T> orNest();

    NestConditional<T> orNest(String column, Object value);

    T and();

    T or();

    default T and(Consumer<Conditional> consumer) {
        consumer.accept(this.and());
        return (T) this;
    }

    default T or(Consumer<Conditional> consumer) {
        consumer.accept(this.or());
        return (T) this;
    }

    T and(String column, String termType, Object value);

    T or(String column, String termType, Object value);

    Accepter<T> getAccepter();

    default T where(String column, Object value) {
        return and(column, TermType.eq, value);
    }

    default T where() {
        return (T) this;
    }

    default  T where(Consumer<Conditional> consumer) {
        consumer.accept(this);
        return (T) this;
    }

    default T and(String column, Object value) {
        return and(column, TermType.eq, value);
    }

    default T is(String column, Object value) {
        return accept(column, TermType.eq, value);
    }

    default T or(String column, Object value) {
        return or(column, TermType.eq, value);
    }

    default T like(String column, Object value) {
        return accept(column, TermType.like, value);
    }

    default T like$(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat(value, "%"));
    }

    default T $like(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat("%", value));
    }

    default T $like$(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat("%", value, "%"));
    }

    default T notLike(String column, Object value) {
        return accept(column, TermType.nlike, value);
    }

    default T gt(String column, Object value) {
        return accept(column, TermType.gt, value);
    }

    default T lt(String column, Object value) {
        return accept(column, TermType.lt, value);
    }

    default T gte(String column, Object value) {
        return accept(column, TermType.gte, value);
    }

    default T lte(String column, Object value) {
        return accept(column, TermType.lte, value);
    }

    default T in(String column, Object value) {
        return accept(column, TermType.in, value);
    }

    default T in(String column, Object... values) {
        return accept(column, TermType.in, values);
    }

    default T in(String column, Collection values) {
        return accept(column, TermType.in, values);
    }

    default T notIn(String column, Object value) {
        return accept(column, TermType.nin, value);
    }

    default T isEmpty(String column) {
        return accept(column, TermType.empty, 1);
    }

    default T notEmpty(String column) {
        return accept(column, TermType.nempty, 1);
    }

    default T isNull(String column) {
        return accept(column, TermType.isnull, 1);
    }

    default T notNull(String column) {
        return accept(column, TermType.notnull, 1);
    }

    default T not(String column, Object value) {
        return accept(column, TermType.not, value);
    }

    default T between(String column, Object between, Object and) {
        return accept(column, TermType.btw, Arrays.asList(between, and));
    }

    default T notBetween(String column, Object between, Object and) {
        return accept(column, TermType.nbtw, Arrays.asList(between, and));
    }

    default T accept(String column, String termType, Object value) {
        return getAccepter().accept(column, termType, value);
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
    T sql(String sql, Object... params);

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
    default T each(String column, Collection list, Function<Conditional<T>, SimpleAccepter<Conditional<T>>> accepterGetter) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(this).accept(column, o));
        return (T) this;
    }

    default T each(String column, String termType, Collection list, Function<Conditional<T>, Accepter<Conditional<T>>> accepterGetter) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(this).accept(column, termType, o));
        return (T) this;
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
    default T each(String column, Collection list, Function<Conditional<T>, SimpleAccepter<Conditional<T>>> accepterGetter, Function<Object, Object> valueMapper) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(this).accept(column, valueMapper.apply(o)));
        return (T) this;
    }

    default T each(String column, String termType, Collection list, Function<Conditional<T>, Accepter<Conditional<T>>> accepterGetter, Function<Object, Object> valueMapper) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(this).accept(column, termType, valueMapper.apply(o)));
        return (T) this;
    }

    /**
     * 遍历一个Map,进行条件追加
     *
     * @param mapParam map参数
     * @param accepter 追加方式函数
     * @return {@link T}
     * @see Function
     * @see Conditional
     * @see org.hsweb.ezorm.core.TermTypeConditionalSupport.SimpleAccepter
     */
    default T each(Map<String, Object> mapParam, Function<Conditional<T>, SimpleAccepter<Conditional<T>>> accepter) {
        if (null != mapParam)
            mapParam.forEach((k, v) -> accepter.apply(this).accept(k, v));
        return (T) this;
    }

    default T each(Map<String, Object> mapParam, String termType, Function<Conditional<T>, Accepter<Conditional<T>>> accepter) {
        if (null != mapParam)
            mapParam.forEach((k, v) -> accepter.apply(this).accept(k, termType, v));
        return (T) this;
    }

    /**
     * 指定一个前置条件,当条件满足的时候,调用回调进行自定义参数<br>
     * 如: query(age>10,query->query.gt("age",10))
     *
     * @param condition 前置条件
     * @param consumer  回调
     * @return {@link T}
     */
    default T when(boolean condition, Consumer<Conditional<T>> consumer) {
        if (condition) {
            consumer.accept(this);
        }
        return (T) this;
    }

    /**
     * 通过BooleanSupplier获取条件,例如<br>
     * query.when(()->age>10,query->query.gt("age",10));
     *
     * @see Conditional#when(boolean, Consumer)
     * @see BooleanSupplier
     */
    default T when(BooleanSupplier condition, Consumer<Conditional<T>> consumer) {
        return when(condition.getAsBoolean(), consumer);
    }

    /**
     * 指定前置条件,列名,参数值,条件构造函数。当条件满足的时候，执行构造器添加条件.例如<br>
     * query.when(age>10,"age",10,query->query::gt);<br>
     * 等同于<br>
     * if(age>10)query.gt(age,10);<br>
     *
     * @param condition 前置条件
     * @param column    要查询的列名
     * @param value     参数值
     * @param accepter  条件构造函数
     * @return {@link T}
     */
    default T when(boolean condition, String column, Object value, Function<Conditional<T>, SimpleAccepter<Conditional<T>>> accepter) {
        if (condition) {
            accepter.apply(this).accept(column, value);
        }
        return (T) this;
    }

    /**
     * 指定列名，参数值，条件判断函数，条件构造函数进行条件添加。如<br>
     * query.when("age",10,value->value>10,query->query::gt)
     *
     * @param column    列名
     * @param value     值
     * @param condition 条件判断函数
     * @param accepter  条件构造函数
     * @return {@link T}
     * @see Conditional#when(boolean, String, Object, Function)
     */
    default <V> T when(String column, V value, Function<V, Boolean> condition, Function<Conditional<T>, SimpleAccepter<Conditional<T>>> accepter) {
        return when(condition.apply(value), column, value, accepter);
    }

    /**
     * 功能与{@link Conditional#when(boolean, String, Object, Function)} 类似,可自定义termType 如:<br>
     * query.when(true,"age","like",10,query->query::or)
     *
     * @param condition 条件
     * @param column    列名
     * @param termType  条件类型
     * @param value     参数
     * @param accepter  条件构造函数
     * @return {@link T}
     * @see Conditional#when(boolean, String, Object, Function)
     */
    default T when(boolean condition, String column, String termType, Object value, Function<Conditional<T>, Accepter<Conditional<T>>> accepter) {
        if (condition) {
            accepter.apply(this).accept(column, termType, value);
        }
        return (T) this;
    }

    /**
     * 功能与{@link Conditional#when(String, Object, Function, Function)} 类似,可自定义termType 如:<br>
     * query.when("age","like",10,value->value==10,query->query::or)
     *
     * @param condition 条件
     * @param column    列名
     * @param termType  条件类型
     * @param value     参数
     * @param accepter  条件构造函数
     * @return {@link T}
     * @see Conditional#when(boolean, String, Object, Function)
     * @see TermType
     */
    default <V> T when(String column, String termType, V value, Function<V, Boolean> condition, Function<Conditional<T>, Accepter<Conditional<T>>> accepter) {
        return when(condition.apply(value), column, termType, value, accepter);
    }

}
