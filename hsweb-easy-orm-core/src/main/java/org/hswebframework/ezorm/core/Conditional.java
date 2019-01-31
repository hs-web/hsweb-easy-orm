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

package org.hswebframework.ezorm.core;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.utils.StringUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Consumer;
import java.util.function.Function;

public interface Conditional<T extends Conditional> extends LogicalOperation<T>, TermTypeConditionalSupport {
    /*
     * 嵌套条件，如: where name = ? or (age > 18 and age <90)
     * */

    NestConditional<T> nest();

    NestConditional<T> nest(String column, Object value);

    NestConditional<T> orNest();

    NestConditional<T> orNest(String column, Object value);

    /*
     * and or 切换
     * */

    T and();

    T or();

    /*
     * 自定义and和or的操作
     * */

    default T and(Consumer<T> consumer) {
        consumer.accept(this.and());
        return (T) this;
    }

    default T or(Consumer<T> consumer) {
        consumer.accept(this.or());
        return (T) this;
    }

    /*
     * 自定义条件类型 and和or的操作
     * */
    T and(String column, String termType, Object value);

    T or(String column, String termType, Object value);

    default <B> T and(StaticMethodReferenceColumn<B> column, String termType, Object value) {
        return and(column.getColumn(), termType, value);
    }

    default <B> T or(StaticMethodReferenceColumn<B> column, String termType, Object value) {
        return or(column.getColumn(), termType, value);
    }

    default T where(String column, Object value) {
        return and(column, TermType.eq, value);
    }

    default <B> T where(StaticMethodReferenceColumn<B> column, Object value) {
        return and(column, TermType.eq, value);
    }

    default <B> T where(MethodReferenceColumn<B> column) {
        return and(column.getColumn(), TermType.eq, column.get());
    }

    default T where() {
        return (T) this;
    }

    default T where(Consumer<Conditional<T>> consumer) {
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

    default T notIn(String column, Object... value) {
        return accept(column, TermType.nin, value);
    }

    default T notIn(String column, Collection values) {
        return accept(column, TermType.nin, values);
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


    /*---------lambda---------*/

    default <B> T and(StaticMethodReferenceColumn<B> column, Object value) {
        return and(column, TermType.eq, value);
    }

    default <B> T and(MethodReferenceColumn<B> column) {
        return and(column.getColumn(), TermType.eq, column.get());
    }

    default <B> T is(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.eq, value);
    }

    default <B> T is(MethodReferenceColumn<B> column) {
        return accept(column.getColumn(), TermType.eq, column.get());
    }

    default <B> T or(StaticMethodReferenceColumn<B> column, Object value) {
        return or(column, TermType.eq, value);
    }

    default <B> T or(MethodReferenceColumn<B> column) {
        return or(column.getColumn(), TermType.eq, column.get());
    }

    default <B> T like(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.like, value);
    }

    default <B> T like(MethodReferenceColumn<B> column) {
        return accept(column.getColumn(), TermType.like, column.get());
    }

    default <B> T like$(StaticMethodReferenceColumn<B> column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat(value, "%"));
    }

    default <B> T like$(MethodReferenceColumn<B> column) {
        Object val = column.get();
        if (val == null)
            return like(column.getColumn(), null);
        return accept(column.getColumn(), TermType.like, StringUtils.concat(val, "%"));
    }

    default <B> T $like(MethodReferenceColumn<B> column) {
        Object val = column.get();
        if (val == null)
            return like(column.getColumn(), null);
        return accept(column.getColumn(), TermType.like, StringUtils.concat("%", val));
    }

    default <B> T $like(StaticMethodReferenceColumn<B> column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat("%", value));
    }

    default <B> T $like$(MethodReferenceColumn<B> column) {
        Object val = column.get();
        if (val == null)
            return like(column.getColumn(), null);
        return accept(column.getColumn(), TermType.like, StringUtils.concat("%", val, "%"));
    }

    default <B> T $like$(StaticMethodReferenceColumn<B> column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat("%", value, "%"));
    }

    default <B> T notLike(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.nlike, value);
    }

    default <B> T notLike(MethodReferenceColumn<B> column) {
        return accept(column, TermType.nlike);
    }

    default <B> T gt(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.gt, value);
    }

    default <B> T gt(MethodReferenceColumn<B> column) {
        return accept(column, TermType.gt);
    }

    default <B> T lt(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.lt, value);
    }

    default <B> T lt(MethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.lt);
    }

    default <B> T gte(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.gte, value);
    }

    default <B> T gte(MethodReferenceColumn<B> column) {
        return accept(column, TermType.gte);
    }

    default <B> T lte(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.lte, value);
    }

    default <B> T lte(MethodReferenceColumn<B> column) {
        return accept(column, TermType.lte);
    }

    default <B> T in(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.in, value);
    }

    default <B> T in(MethodReferenceColumn<B> column) {
        return accept(column, TermType.in);
    }

    default <B> T in(StaticMethodReferenceColumn<B> column, Object... values) {
        return accept(column, TermType.in, values);
    }

    default <B> T in(StaticMethodReferenceColumn<B> column, Collection values) {
        return accept(column, TermType.in, values);
    }

    default <B> T notIn(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.nin, value);
    }

    default <B> T notIn(MethodReferenceColumn<B> column) {
        return accept(column, TermType.nin);
    }

    default <B> T notIn(StaticMethodReferenceColumn<B> column, Object... value) {
        return accept(column, TermType.nin, value);
    }

    default <B> T notIn(StaticMethodReferenceColumn<B> column, Collection values) {
        return accept(column, TermType.nin, values);
    }

    default <B> T isEmpty(StaticMethodReferenceColumn<B> column) {
        return accept(column, TermType.empty, 1);
    }

    default <B> T notEmpty(StaticMethodReferenceColumn<B> column) {
        return accept(column, TermType.nempty, 1);
    }

    default <B> T isNull(StaticMethodReferenceColumn<B> column) {
        return accept(column, TermType.isnull, 1);
    }

    default <B> T notNull(StaticMethodReferenceColumn<B> column) {
        return accept(column, TermType.notnull, 1);
    }

    default <B> T not(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.not, value);
    }

    default <B> T not(MethodReferenceColumn<B> column) {
        return accept(column, TermType.not);
    }

    default <B> T between(StaticMethodReferenceColumn<B> column, Object between, Object and) {
        return accept(column, TermType.btw, Arrays.asList(between, and));
    }

    default <B> T between(MethodReferenceColumn<B> column, Function<B, Object> between, Function<B, Object> and) {
        B value = column.get();
        return accept(column.getColumn(), TermType.btw, Arrays.asList(between.apply(value), and.apply(value)));
    }

    default <B> T notBetween(MethodReferenceColumn<B> column, Function<B, Object> between, Function<B, Object> and) {
        B value = column.get();
        return accept(column.getColumn(), TermType.nbtw, Arrays.asList(between.apply(value), and.apply(value)));
    }

    default <B> T notBetween(StaticMethodReferenceColumn<B> column, Object between, Object and) {
        return accept(column, TermType.nbtw, Arrays.asList(between, and));
    }

    default T accept(String column, String termType, Object value) {
        return getAccepter().accept(column, termType, value);
    }

    default <B> T accept(StaticMethodReferenceColumn<B> column, String termType, Object value) {
        return getAccepter().accept(column.getColumn(), termType, value);
    }

    default <B> T accept(MethodReferenceColumn<B> column, String termType) {
        return getAccepter().accept(column.getColumn(), termType, column.get());
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

    Accepter<T, Object> getAccepter();

    default T accept(TermSupplier supplier) {
        Term term = new Term();
        term.setColumn(supplier.getColumn());
        term.setValue(supplier.getValue());
        if (null != supplier.getOptions())
            term.setOptions(supplier.getOptions());
        term.setTermType(supplier.getTermType());
        return this.accept(term);
    }

    T accept(Term term);

}
