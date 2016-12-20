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

public interface ConditionalFromBean<T extends TermTypeConditionalFromBeanSupport> extends TermTypeConditionalFromBeanSupport {

    NestConditionalFromBean<T> nest();

    NestConditionalFromBean<T> nest(String column);

    NestConditionalFromBean<T> orNest();

    NestConditionalFromBean<T> orNest(String column);

    T and();

    T or();

    T and(String column, String termType);

    T or(String column, String termType);

    TermTypeConditionalSupport.Accepter<T> getAccepter();

    default T where(String column) {
        return and(column, TermType.eq);
    }

    default T where() {
        return (T) this;
    }

    default T and(String column) {
        return and(column, TermType.eq);
    }

    default T or(String column) {
        return or(column, TermType.eq);
    }

    default T like(String column) {
        return accept(column, TermType.like);
    }

    default T like$(String column) {
        Object value = getValue(column);
        if (value == null)
            return like(column);
        return accept(column, TermType.like, StringUtils.concat(value, "%"));
    }

    default T $like(String column) {
        Object value = getValue(column);
        if (value == null)
            return like(column);
        return accept(column, TermType.like, StringUtils.concat("%"));
    }

    default T $like$(String column) {
        Object value = getValue(column);
        if (value == null)
            return like(column);
        return accept(column, TermType.like, StringUtils.concat("%", "%"));
    }

    default T notLike(String column) {
        return accept(column, TermType.nlike);
    }

    default T gt(String column) {
        return accept(column, TermType.gt);
    }

    default T lt(String column) {
        return accept(column, TermType.lt);
    }

    default T gte(String column) {
        return accept(column, TermType.gte);
    }

    default T lte(String column) {
        return accept(column, TermType.lte);
    }

    default T in(String column) {
        return accept(column, TermType.in);
    }

    default T in(String column, Object... values) {
        return accept(column, TermType.in, values);
    }

    default T in(String column, Collection values) {
        return accept(column, TermType.in, values);
    }

    default T notIn(String column, Object... values) {
        return accept(column, TermType.nin, values);
    }

    default T notIn(String column, Collection values) {
        return accept(column, TermType.nin, values);
    }

    T sql(String sql, Object... params);

    default T notIn(String column) {
        return accept(column, TermType.nin);
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

    default T not(String column) {
        return accept(column, TermType.not);
    }

    default T between(String column, Object between, Object and) {
        return accept(column, TermType.btw, Arrays.asList(between, and));
    }

    default T notBetween(String column, Object between, Object and) {
        return accept(column, TermType.nbtw, Arrays.asList(between, and));
    }

    default T accept(String column, String termType) {
        return accept(column, termType, getValue(column));
    }

    default T accept(String column, String termType, Object value) {
        return getAccepter().accept(column, termType, value);
    }

    /**
     * @see Conditional#each(String, Collection, Function)
     */
    default T each(String column, Collection list, Function<ConditionalFromBean<T>, TermTypeConditionalSupport.SimpleAccepter<ConditionalFromBean<T>>> accepter) {
        list.forEach(o -> accepter.apply(this).accept(column, o));
        return (T) this;
    }


    /**
     * @see Conditional#each(String, String, Collection, Function)
     */
    default T each(String column, Collection list, Function<ConditionalFromBean<T>, TermTypeConditionalSupport.SimpleAccepter<ConditionalFromBean<T>>> accepter, Function<Object, Object> valueMapper) {
        list.forEach(o -> accepter.apply(this).accept(column, valueMapper.apply(o)));
        return (T) this;
    }

    /**
     * @see Conditional#each(String, Collection, Function)
     */
    default T each(Map<String, Object> mapParam, Function<ConditionalFromBean<T>, TermTypeConditionalSupport.SimpleAccepter<ConditionalFromBean<T>>> accepter) {
        mapParam.forEach((k, v) -> accepter.apply(this).accept(k, v));
        return (T) this;
    }

    /**
     * @see Conditional#each(String, String, Collection, Function)
     */
    default T each(String column, String termType, Collection list, Function<ConditionalFromBean<T>, TermTypeConditionalSupport.Accepter<ConditionalFromBean<T>>> accepter) {
        list.forEach(o -> accepter.apply(this).accept(column, termType, o));
        return (T) this;
    }

    /**
     * @see Conditional#each(Map, Function)
     */
    default T each(String column, String termType, Collection list, Function<ConditionalFromBean<T>, TermTypeConditionalSupport.Accepter<ConditionalFromBean<T>>> accepter, Function<Object, Object> valueMapper) {
        list.forEach(o -> accepter.apply(this).accept(column, termType, valueMapper.apply(o)));
        return (T) this;
    }

    /**
     * @see Conditional#each(Map, String, Function)
     */
    default T each(Map<String, Object> mapParam, String termType, Function<ConditionalFromBean<T>, TermTypeConditionalSupport.Accepter<ConditionalFromBean<T>>> accepter) {
        mapParam.forEach((k, v) -> accepter.apply(this).accept(k, termType, v));
        return (T) this;
    }

    /**
     * @see Conditional#when(boolean, Consumer)
     */
    default T when(boolean condition, Consumer<ConditionalFromBean<T>> consumer) {
        if (condition) {
            consumer.accept(this);
        }
        return (T) this;
    }

    /**
     * @see Conditional#when(BooleanSupplier, Consumer)
     */
    default T when(BooleanSupplier condition, Consumer<ConditionalFromBean<T>> consumer) {
        return when(condition.getAsBoolean(), consumer);
    }

    /**
     * @see Conditional#when(boolean, String, Object, Function)
     */
    default T when(boolean condition, String column, Function<ConditionalFromBean<T>, TermTypeConditionalSupport.SimpleAccepter<ConditionalFromBean<T>>> accepter) {
        if (condition) {
            accepter.apply(this).accept(column, getValue(column));
        }
        return (T) this;
    }

    /**
     * @see Conditional#when(String, Object, Function, Function)
     */
    default T when(String column, Function<Object, Boolean> condition, Function<ConditionalFromBean<T>, TermTypeConditionalSupport.SimpleAccepter<ConditionalFromBean<T>>> accepter) {
        Object value = getValue(column);
        return when(condition.apply(value), column, accepter);
    }

    /**
     * @see Conditional#when(boolean, String, String, Object, Function)
     */
    default T when(boolean condition, String column, String termType, Function<ConditionalFromBean<T>, TermTypeConditionalSupport.Accepter<ConditionalFromBean<T>>> accepter) {
        if (condition) {
            accepter.apply(this).accept(column, termType, getValue(column));
        }
        return (T) this;
    }

    /**
     * @see Conditional#when(String, String, Object, Function, Function)
     */
    default T when(String column, String termType, Function<Object, Boolean> condition, Function<ConditionalFromBean<T>, TermTypeConditionalSupport.Accepter<ConditionalFromBean<T>>> accepter) {
        Object value = getValue(column);
        return when(condition.apply(value), column, termType, accepter);
    }
}
