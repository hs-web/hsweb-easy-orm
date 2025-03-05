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

import org.hswebframework.ezorm.core.param.Param;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.core.utils.StringUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

@SuppressWarnings("all")
public interface Conditional<T extends Conditional<?>> extends LogicalOperation<T>, TermTypeConditionalSupport {

    /**
     * 创建一个嵌套条件，使用AND连接符。
     * <p>
     * 嵌套构造结束后请调用{@link NestConditional#end()}结束嵌套
     * <p>
     * 如: where name = ? or (age > 18 and age <90)
     *
     * @return 嵌套条件构造器
     * @see Conditional#nest(Consumer)
     */
    NestConditional<T> nest();

    /**
     * 创建一个嵌套条件，使用OR连接符。
     * <p>
     * 嵌套构造结束后请调用{@link NestConditional#end()}结束嵌套
     *
     * @return 嵌套条件构造器
     */
    NestConditional<T> orNest();

    /**
     * 嵌套条件
     * <pre>{@code
     *
     * // 对应SQL: (  name = ?  )
     * nest(n-> n.is(MyEntity::getName,name))
     *
     * }</pre>
     *
     * @param consumer 条件消费者
     * @return 当前条件构造器
     */
    default T nest(Consumer<NestConditional<T>> consumer) {
        NestConditional<T> nest = nest();
        consumer.accept(nest);
        return nest.end();
    }

    /*
     * and or 切换
     * */

    /**
     * 设置下一个条件为AND连接
     *
     * @return 当前条件构造器
     */
    T and();

    /**
     * 设置下一个条件为OR连接
     *
     * @return 当前条件构造器
     */
    T or();


    /**
     * 使用AND连接并应用自定义条件
     *
     * @param consumer 条件消费者
     * @return 当前条件构造器
     */
    default T and(Consumer<T> consumer) {
        consumer.accept(this.and());
        return castSelf();
    }

    /**
     * 使用OR连接并应用自定义条件
     *
     * @param consumer 条件消费者
     * @return 当前条件构造器
     */
    default T or(Consumer<T> consumer) {
        consumer.accept(this.or());
        return castSelf();
    }


    /**
     * 使用AND连接自定义条件类型
     *
     * @param column   列名
     * @param termType 条件类型
     * @param value    条件值
     * @return 当前条件构造器
     */
    T and(String column, String termType, Object value);

    /**
     * 使用OR连接自定义条件类型
     *
     * @param column   列名
     * @param termType 条件类型
     * @param value    条件值
     * @return 当前条件构造器
     */
    T or(String column, String termType, Object value);

    /**
     * 使用AND连接自定义条件类型（方法引用方式）
     *
     * @param column   列引用
     * @param termType 条件类型
     * @param value    条件值
     * @return 当前条件构造器
     */
    default <B> T and(StaticMethodReferenceColumn<B> column, String termType, Object value) {
        return and(column.getColumn(), termType, value);
    }

    /**
     * 使用OR连接自定义条件类型（方法引用方式）
     *
     * @param column   列引用
     * @param termType 条件类型
     * @param value    条件值
     * @return 当前条件构造器
     */
    default <B> T or(StaticMethodReferenceColumn<B> column, String termType, Object value) {
        return or(column.getColumn(), termType, value);
    }

    /**
     * 添加WHERE条件，等同于AND条件
     *
     * @param column 列名
     * @param value  条件值
     * @return 当前条件构造器
     */
    default T where(String column, Object value) {
        return and(column, TermType.eq, value);
    }

    /**
     * 添加WHERE条件，等同于AND条件（方法引用方式）
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T where(StaticMethodReferenceColumn<B> column, Object value) {
        return and(column, TermType.eq, value);
    }

    /**
     * 添加WHERE条件，等同于AND条件（方法引用方式）
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T where(MethodReferenceColumn<B> column) {
        return and(column.getColumn(), TermType.eq, column.get());
    }

    /**
     * 开始一个空的WHERE条件
     *
     * @return 当前条件构造器
     */
    default T where() {
        return castSelf();
    }

    /**
     * 使用自定义消费者构建WHERE条件
     *
     * @param consumer 条件消费者
     * @return 当前条件构造器
     */
    default T where(Consumer<Conditional<T>> consumer) {
        consumer.accept(this);
        return castSelf();
    }

    /**
     * 使用AND连接自定义Term条件
     *
     * @param termSupplier Term提供者
     * @return 当前条件构造器
     */
    default T and(Supplier<Term> termSupplier) {
        Term term = termSupplier.get();
        term.setType(Term.Type.and);
        accept(term);
        return castSelf();
    }

    /**
     * 使用OR连接自定义Term条件
     *
     * @param termSupplier Term提供者
     * @return 当前条件构造器
     */
    default T or(Supplier<Term> termSupplier) {
        Term term = termSupplier.get();
        term.setType(Term.Type.or);
        accept(term);
        return castSelf();
    }

    /**
     * 使用AND连接等于条件
     *
     * @param column 列名
     * @param value  条件值
     * @return 当前条件构造器
     */
    default T and(String column, Object value) {
        return and(column, TermType.eq, value);
    }

    /**
     * 使用OR连接等于条件
     *
     * @param column 列名
     * @param value  条件值
     * @return 当前条件构造器
     */
    default T or(String column, Object value) {
        return or(column, TermType.eq, value);
    }

    /**
     * 追加等于条件: column = ?
     *
     * @param column 列名
     * @param value  条件值
     * @return this
     */
    default T is(String column, Object value) {
        return accept(column, TermType.eq, value);
    }

    /**
     * 追加like条件: column like ?
     *
     * @param column 列名
     * @param value  条件值
     * @return this
     */
    default T like(String column, Object value) {
        return accept(column, TermType.like, value);
    }


    /**
     * 追加like条件，以%结尾: column like concat(?, '%')
     *
     * @param column 列名
     * @param value  条件值
     * @return this
     */
    default T like$(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat(value, "%"));
    }

    /**
     * 追加like条件，以%开头: column like concat('%',?)
     *
     * @param column 列名
     * @param value  条件值
     * @return this
     */
    default T $like(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat("%", value));
    }

    /**
     * 追加like条件，以%开头和结尾: column like concat('%',?,'%')
     *
     * @param column 列名
     * @param value  条件值
     * @return this
     */
    default T $like$(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat("%", value, "%"));
    }

    /**
     * 追加not like条件: column not like ?
     *
     * @param column 列名
     * @param value  条件值
     * @return this
     */
    default T notLike(String column, Object value) {
        return accept(column, TermType.nlike, value);
    }

    /**
     * 追加大于条件: column > ?
     *
     * @param column 列名
     * @param value  条件值
     * @return this
     */
    default T gt(String column, Object value) {
        return accept(column, TermType.gt, value);
    }

    /**
     * 追加小于条件: column < ?
     *
     * @param column 列名
     * @param value  条件值
     * @return this
     */
    default T lt(String column, Object value) {
        return accept(column, TermType.lt, value);
    }

    /**
     * 追加大于等于条件: column >= ?
     *
     * @param column 列名
     * @param value  条件值
     * @return this
     */
    default T gte(String column, Object value) {
        return accept(column, TermType.gte, value);
    }

    /**
     * 追加小于等于条件: column <= ?
     *
     * @param column 列名
     * @param value  条件值
     * @return this
     */
    default T lte(String column, Object value) {
        return accept(column, TermType.lte, value);
    }

    /**
     * 追加IN条件: column in (?)
     *
     * @param column 列名
     * @param value  条件值
     * @return 当前条件构造器
     */
    default T in(String column, Object value) {
        return accept(column, TermType.in, value);
    }

    /**
     * 追加IN条件: column in (?,?,?)
     *
     * @param column 列名
     * @param values 条件值数组
     * @return 当前条件构造器
     */
    default T in(String column, Object... values) {
        return accept(column, TermType.in, values);
    }

    /**
     * 追加IN条件: column in (collection)
     *
     * @param column 列名
     * @param values 条件值集合
     * @return 当前条件构造器
     */
    default T in(String column, Collection values) {
        return accept(column, TermType.in, values);
    }

    /**
     * 追加NOT IN条件: column not in (?)
     *
     * @param column 列名
     * @param value  条件值
     * @return 当前条件构造器
     */
    default T notIn(String column, Object value) {
        return accept(column, TermType.nin, value);
    }

    /**
     * 追加NOT IN条件: column not in (?,?,?)
     *
     * @param column 列名
     * @param value  条件值数组
     * @return 当前条件构造器
     */
    default T notIn(String column, Object... value) {
        return accept(column, TermType.nin, value);
    }

    /**
     * 追加NOT IN条件: column not in (collection)
     *
     * @param column 列名
     * @param values 条件值集合
     * @return 当前条件构造器
     */
    default T notIn(String column, Collection values) {
        return accept(column, TermType.nin, values);
    }

    /**
     * 追加为空条件: column = ''
     *
     * @param column 列名
     * @return 当前条件构造器
     */
    default T isEmpty(String column) {
        return accept(column, TermType.empty, 1);
    }

    /**
     * 追加不为空条件: column != ''
     *
     * @param column 列名
     * @return 当前条件构造器
     */
    default T notEmpty(String column) {
        return accept(column, TermType.nempty, 1);
    }

    /**
     * 追加为NULL条件: column is null
     *
     * @param column 列名
     * @return 当前条件构造器
     */
    default T isNull(String column) {
        return accept(column, TermType.isnull, 1);
    }

    /**
     * 追加不为NULL条件: column is not null
     *
     * @param column 列名
     * @return 当前条件构造器
     */
    default T notNull(String column) {
        return accept(column, TermType.notnull, 1);
    }

    /**
     * 追加不等于条件: column != ?
     *
     * @param column 列名
     * @param value  条件值
     * @return 当前条件构造器
     */
    default T not(String column, Object value) {
        return accept(column, TermType.not, value);
    }

    /**
     * 追加BETWEEN条件: column between ? and ?
     *
     * @param column  列名
     * @param between 开始值
     * @param and     结束值
     * @return 当前条件构造器
     */
    default T between(String column, Object between, Object and) {
        return accept(column, TermType.btw, Arrays.asList(between, and));
    }

    /**
     * 追加NOT BETWEEN条件: column not between ? and ?
     *
     * @param column  列名
     * @param between 开始值
     * @param and     结束值
     * @return 当前条件构造器
     */
    default T notBetween(String column, Object between, Object and) {
        return accept(column, TermType.nbtw, Arrays.asList(between, and));
    }


    /*---------lambda---------*/

    /**
     * 使用AND连接等于条件（静态方法引用方式）
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T and(StaticMethodReferenceColumn<B> column, Object value) {
        return and(column, TermType.eq, value);
    }

    /**
     * 使用AND连接等于条件（方法引用方式）
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T and(MethodReferenceColumn<B> column) {
        return and(column.getColumn(), TermType.eq, column.get());
    }

    /**
     * 追加等于条件（静态方法引用方式）: column = ?
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T is(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.eq, value);
    }

    /**
     * 追加等于条件（方法引用方式）: column = ?
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T is(MethodReferenceColumn<B> column) {
        return accept(column.getColumn(), TermType.eq, column.get());
    }

    /**
     * 使用OR连接等于条件（静态方法引用方式）
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T or(StaticMethodReferenceColumn<B> column, Object value) {
        return or(column, TermType.eq, value);
    }

    /**
     * 使用OR连接等于条件（方法引用方式）
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T or(MethodReferenceColumn<B> column) {
        return or(column.getColumn(), TermType.eq, column.get());
    }

    /**
     * 追加LIKE条件（静态方法引用方式）: column like ?
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T like(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.like, value);
    }

    /**
     * 追加LIKE条件（方法引用方式）: column like ?
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T like(MethodReferenceColumn<B> column) {
        return accept(column.getColumn(), TermType.like, column.get());
    }

    /**
     * 追加LIKE条件，以%结尾（静态方法引用方式）: column like concat(?, '%')
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T like$(StaticMethodReferenceColumn<B> column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat(value, "%"));
    }

    /**
     * 追加LIKE条件，以%结尾（方法引用方式）: column like concat(?, '%')
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T like$(MethodReferenceColumn<B> column) {
        Object val = column.get();
        if (val == null)
            return like(column.getColumn(), null);
        return accept(column.getColumn(), TermType.like, StringUtils.concat(val, "%"));
    }

    /**
     * 追加LIKE条件，以%开头（方法引用方式）: column like concat('%', ?)
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T $like(MethodReferenceColumn<B> column) {
        Object val = column.get();
        if (val == null)
            return like(column.getColumn(), null);
        return accept(column.getColumn(), TermType.like, StringUtils.concat("%", val));
    }

    /**
     * 追加LIKE条件，以%开头（静态方法引用方式）: column like concat('%', ?)
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T $like(StaticMethodReferenceColumn<B> column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat("%", value));
    }

    /**
     * 追加LIKE条件，以%开头和结尾（方法引用方式）: column like concat('%', ?, '%')
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T $like$(MethodReferenceColumn<B> column) {
        Object val = column.get();
        if (val == null)
            return like(column.getColumn(), null);
        return accept(column.getColumn(), TermType.like, StringUtils.concat("%", val, "%"));
    }

    /**
     * 追加LIKE条件，以%开头和结尾（静态方法引用方式）: column like concat('%', ?, '%')
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T $like$(StaticMethodReferenceColumn<B> column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, StringUtils.concat("%", value, "%"));
    }

    /**
     * 追加NOT LIKE条件（静态方法引用方式）: column not like ?
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T notLike(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.nlike, value);
    }

    /**
     * 追加NOT LIKE条件（方法引用方式）: column not like ?
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T notLike(MethodReferenceColumn<B> column) {
        return accept(column, TermType.nlike);
    }

    /**
     * 追加大于条件（静态方法引用方式）: column > ?
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T gt(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.gt, value);
    }

    /**
     * 追加大于条件（方法引用方式）: column > ?
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T gt(MethodReferenceColumn<B> column) {
        return accept(column, TermType.gt);
    }

    /**
     * 追加小于条件（静态方法引用方式）: column < ?
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T lt(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.lt, value);
    }

    /**
     * 追加小于条件（方法引用方式）: column < ?
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T lt(MethodReferenceColumn<B> column) {
        return accept(column, TermType.lt);
    }

    /**
     * 追加大于等于条件（静态方法引用方式）: column >= ?
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T gte(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.gte, value);
    }

    /**
     * 追加大于等于条件（方法引用方式）: column >= ?
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T gte(MethodReferenceColumn<B> column) {
        return accept(column, TermType.gte);
    }

    /**
     * 追加小于等于条件（静态方法引用方式）: column <= ?
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T lte(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.lte, value);
    }

    /**
     * 追加小于等于条件（方法引用方式）: column <= ?
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T lte(MethodReferenceColumn<B> column) {
        return accept(column, TermType.lte);
    }

    /**
     * 追加IN条件（静态方法引用方式）: column in (?)
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T in(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.in, value);
    }

    /**
     * 追加IN条件（方法引用方式）: column in (?)
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T in(MethodReferenceColumn<B> column) {
        return accept(column, TermType.in);
    }

    /**
     * 追加IN条件（静态方法引用方式）: column in (?,?,?)
     *
     * @param column 列引用
     * @param values 条件值数组
     * @return 当前条件构造器
     */
    default <B> T in(StaticMethodReferenceColumn<B> column, Object... values) {
        return accept(column, TermType.in, values);
    }

    /**
     * 追加IN条件（静态方法引用方式）: column in (collection)
     *
     * @param column 列引用
     * @param values 条件值集合
     * @return 当前条件构造器
     */
    default <B> T in(StaticMethodReferenceColumn<B> column, Collection<?> values) {
        return accept(column, TermType.in, values);
    }

    /**
     * 追加NOT IN条件（静态方法引用方式）: column not in (?)
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T notIn(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.nin, value);
    }

    /**
     * 追加NOT IN条件（方法引用方式）: column not in (?)
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T notIn(MethodReferenceColumn<B> column) {
        return accept(column, TermType.nin);
    }

    /**
     * 追加NOT IN条件（静态方法引用方式）: column not in (?,?,?)
     *
     * @param column 列引用
     * @param value  条件值数组
     * @return 当前条件构造器
     */
    default <B> T notIn(StaticMethodReferenceColumn<B> column, Object... value) {
        return accept(column, TermType.nin, value);
    }

    /**
     * 追加NOT IN条件（静态方法引用方式）: column not in (collection)
     *
     * @param column 列引用
     * @param values 条件值集合
     * @return 当前条件构造器
     */
    default <B> T notIn(StaticMethodReferenceColumn<B> column, Collection<?> values) {
        return accept(column, TermType.nin, values);
    }

    /**
     * 追加为空条件（静态方法引用方式）: column = ''
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T isEmpty(StaticMethodReferenceColumn<B> column) {
        return accept(column, TermType.empty, 1);
    }

    /**
     * 追加不为空条件（静态方法引用方式）: column != ''
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T notEmpty(StaticMethodReferenceColumn<B> column) {
        return accept(column, TermType.nempty, 1);
    }

    /**
     * 追加为NULL条件（静态方法引用方式）: column is null
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T isNull(StaticMethodReferenceColumn<B> column) {
        return accept(column, TermType.isnull, 1);
    }

    /**
     * 追加不为NULL条件（静态方法引用方式）: column is not null
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T notNull(StaticMethodReferenceColumn<B> column) {
        return accept(column, TermType.notnull, 1);
    }

    /**
     * 追加不等于条件（静态方法引用方式）: column != ?
     *
     * @param column 列引用
     * @param value  条件值
     * @return 当前条件构造器
     */
    default <B> T not(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.not, value);
    }

    /**
     * 追加不等于条件（方法引用方式）: column != ?
     *
     * @param column 列引用
     * @return 当前条件构造器
     */
    default <B> T not(MethodReferenceColumn<B> column) {
        return accept(column, TermType.not);
    }

    /**
     * 追加BETWEEN条件（静态方法引用方式）: column between ? and ?
     *
     * @param column  列引用
     * @param between 开始值
     * @param and     结束值
     * @return 当前条件构造器
     */
    default <B> T between(StaticMethodReferenceColumn<B> column, Object between, Object and) {
        return accept(column, TermType.btw, Arrays.asList(between, and));
    }

    /**
     * 追加BETWEEN条件（方法引用方式）: column between ? and ?
     *
     * @param column  列引用
     * @param between 开始值函数
     * @param and     结束值函数
     * @return 当前条件构造器
     */
    default <B> T between(MethodReferenceColumn<B> column, Function<B, Object> between, Function<B, Object> and) {
        B value = column.get();
        return accept(column.getColumn(), TermType.btw, Arrays.asList(between.apply(value), and.apply(value)));
    }

    /**
     * 追加NOT BETWEEN条件（方法引用方式）: column not between ? and ?
     *
     * <pre>{@code
     *  notBetween(Query::getRange,Range::getMin,Range::getMax)
     * }</pre>
     *
     * @param column  列引用
     * @param between 开始值函数
     * @param and     结束值函数
     * @return 当前条件构造器
     */
    default <B> T notBetween(MethodReferenceColumn<B> column, Function<B, Object> between, Function<B, Object> and) {
        B value = column.get();
        return accept(column.getColumn(), TermType.nbtw, Arrays.asList(between.apply(value), and.apply(value)));
    }

    /**
     * 追加NOT BETWEEN条件（静态方法引用方式）: column not between ? and ?
     * <pre>{@code
     *  notBetween(UserEntity::getAge,10,30)
     * }</pre>
     *
     * @param column  列引用
     * @param between 开始值
     * @param and     结束值
     * @return 当前条件构造器
     */
    default <B> T notBetween(StaticMethodReferenceColumn<B> column, Object between, Object and) {
        return accept(column, TermType.nbtw, Arrays.asList(between, and));
    }

    /**
     * 接受一个条件
     *
     * <pre>{@code
     *  accept("name","custom-term-type",value)
     * }</pre>
     *
     * @param column   列名
     * @param termType 条件类型
     * @param value    条件值
     * @return 当前条件构造器
     */
    default T accept(String column, String termType, Object value) {
        return getAccepter().accept(column, termType, value);
    }

    /**
     * 接受一个条件（静态方法引用方式）
     * <pre>{@code
     *  accept(user::getName,"custom-term-type",value)
     * }</pre>
     *
     * @param column   列引用
     * @param termType 条件类型
     * @param value    条件值
     * @return 当前条件构造器
     */
    default <B> T accept(StaticMethodReferenceColumn<B> column, String termType, Object value) {
        return getAccepter().accept(column.getColumn(), termType, value);
    }

    /**
     * 接受一个条件（方法引用方式）
     *
     * <pre>{@code
     *  accept(user::getName,"custom-term-type")
     * }</pre>
     *
     * @param column   列引用
     * @param termType 条件类型
     * @return 当前条件构造器
     */
    default <B> T accept(MethodReferenceColumn<B> column, String termType) {
        return getAccepter().accept(column.getColumn(), termType, column.get());
    }

    /**
     * 获取条件接收器
     *
     * @return 条件接收器
     */
    Accepter<T, Object> getAccepter();

    /**
     * 接受一个Term条件
     *
     * @param term 条件
     * @return 当前条件构造器
     */
    T accept(Term term);

    /**
     * 接受Param参数中的所有条件
     *
     * @param param 参数
     * @return 当前条件构造器
     */
    default T accept(Param param) {
        for (Term term : param.getTerms()) {
            accept(term);
        }
        return castSelf();
    }
}
