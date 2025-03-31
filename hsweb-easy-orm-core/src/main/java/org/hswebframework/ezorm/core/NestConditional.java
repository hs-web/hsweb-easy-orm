package org.hswebframework.ezorm.core;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.core.utils.StringUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Function;
import java.util.function.Supplier;

@SuppressWarnings("all")
public interface NestConditional<T extends TermTypeConditionalSupport> extends LogicalOperation<NestConditional<T>>, TermTypeConditionalSupport {

    /**
     * 结束当前嵌套条件并返回父级条件构造器
     * 
     * @return 父级条件构造器
     */
    T end();

    /**
     * 在嵌套条件中创建一个新的嵌套条件，使用AND连接符
     * 
     * @return 新的嵌套条件构造器
     */
    NestConditional<? extends NestConditional<T>> nest();

    /**
     * 在嵌套条件中创建一个新的嵌套条件，并添加一个等于条件，使用AND连接符
     * 
     * @param column 列名
     * @param value 条件值
     * @return 新的嵌套条件构造器
     */
    NestConditional<? extends NestConditional<T>> nest(String column, Object value);

    /**
     * 在嵌套条件中创建一个新的嵌套条件，使用OR连接符
     * 
     * @return 新的嵌套条件构造器
     */
    NestConditional<? extends NestConditional<T>> orNest();

    /**
     * 在嵌套条件中创建一个新的嵌套条件，并添加一个等于条件，使用OR连接符
     * 
     * @param column 列名
     * @param value 条件值
     * @return 新的嵌套条件构造器
     */
    NestConditional<? extends NestConditional<T>> orNest(String column, Object value);

    /**
     * 设置下一个条件为AND连接
     * 
     * @return 当前嵌套条件构造器
     */
    NestConditional<T> and();

    /**
     * 设置下一个条件为OR连接
     * 
     * @return 当前嵌套条件构造器
     */
    NestConditional<T> or();

    /**
     * 使用AND连接自定义条件类型
     * 
     * @param column 列名
     * @param termType 条件类型
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    NestConditional<T> and(String column, String termType, Object value);

    /**
     * 使用OR连接自定义条件类型
     * 
     * @param column 列名
     * @param termType 条件类型
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    NestConditional<T> or(String column, String termType, Object value);

    /**
     * 使用AND连接自定义Term条件
     * 
     * @param termSupplier Term提供者
     * @return 当前嵌套条件构造器
     */
    default T and(Supplier<Term> termSupplier) {
        Term term = termSupplier.get();
        term.setType(Term.Type.and);
        accept(term);
        return (T) this;
    }

    /**
     * 使用OR连接自定义Term条件
     * 
     * @param termSupplier Term提供者
     * @return 当前嵌套条件构造器
     */
    default T or(Supplier<Term> termSupplier) {
        Term term = termSupplier.get();
        term.setType(Term.Type.or);
        accept(term);
        return (T) this;
    }

    /**
     * 使用AND连接自定义条件类型（方法引用方式）
     * 
     * @param column 列引用
     * @param termType 条件类型
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> and(StaticMethodReferenceColumn<B> column, String termType, Object value) {
        return and(column.getColumn(), termType, value);
    }

    /**
     * 使用OR连接自定义条件类型（方法引用方式）
     * 
     * @param column 列引用
     * @param termType 条件类型
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> or(StaticMethodReferenceColumn<B> column, String termType, Object value) {
        return or(column.getColumn(), termType, value);
    }

    /**
     * 使用AND连接等于条件
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> and(String column, Object value) {
        and();
        return and(column, TermType.eq, value);
    }

    /**
     * 使用OR连接等于条件
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> or(String column, Object value) {
        or();
        return or(column, TermType.eq, value);
    }

    /**
     * 追加LIKE条件: column like ?
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> like(String column, Object value) {
        return accept(column, TermType.like, value);
    }

    /**
     * 追加LIKE条件，以%结尾: column like concat(?, '%')
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> like$(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, String.valueOf(value).concat("%"));
    }

    /**
     * 追加LIKE条件，以%开头: column like concat('%', ?)
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> $like(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, "%".concat(String.valueOf(value)));
    }

    /**
     * 追加LIKE条件，以%开头和结尾: column like concat('%', ?, '%')
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> $like$(String column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, "%".concat(String.valueOf(value)).concat("%"));
    }

    /**
     * 追加等于条件: column = ?
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> is(String column, Object value) {
        return accept(column, TermType.eq, value);
    }

    /**
     * 追加不匹配条件: column not like ?
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> notLike(String column, Object value) {
        return accept(column, TermType.nlike, value);
    }

    /**
     * 追加大于条件: column > ?
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> gt(String column, Object value) {
        return accept(column, TermType.gt, value);
    }

    /**
     * 追加小于条件: column < ?
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> lt(String column, Object value) {
        return accept(column, TermType.lt, value);
    }

    /**
     * 追加大于等于条件: column >= ?
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> gte(String column, Object value) {
        return accept(column, TermType.gte, value);
    }

    /**
     * 追加小于等于条件: column <= ?
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> lte(String column, Object value) {
        return accept(column, TermType.lte, value);
    }

    /**
     * 追加IN条件: column in (?)
     * 
     * @param column 列名
     * @param values 多个条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> in(String column, Object... values) {
        return accept(column, TermType.in, values);
    }

    /**
     * 追加IN条件: column in (?)
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> in(String column, Object value) {
        return accept(column, TermType.in, value);
    }

    /**
     * 追加IN条件: column in (?)
     * 
     * @param column 列名
     * @param values 条件值集合
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> in(String column, Collection values) {
        return accept(column, TermType.in, values);
    }

    /**
     * 追加NOT IN条件: column not in (?)
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> notIn(String column, Object value) {
        return accept(column, TermType.nin, value);
    }

    /**
     * 追加为空条件: column = '' or column is null
     * 
     * @param column 列名
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> isEmpty(String column) {
        return accept(column, TermType.empty, 1);
    }

    /**
     * 追加不为空条件: column != '' and column is not null
     * 
     * @param column 列名
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> notEmpty(String column) {
        return accept(column, TermType.nempty, 1);
    }

    /**
     * 追加为NULL条件: column is null
     * 
     * @param column 列名
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> isNull(String column) {
        return accept(column, TermType.isnull, 1);
    }

    /**
     * 追加不为NULL条件: column is not null
     * 
     * @param column 列名
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> notNull(String column) {
        return accept(column, TermType.notnull, 1);
    }

    /**
     * 追加不等于条件: column != ?
     * 
     * @param column 列名
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> not(String column, Object value) {
        return accept(column, TermType.not, value);
    }

    /**
     * 追加BETWEEN条件: column between ? and ?
     * 
     * @param column 列名
     * @param between 起始值
     * @param and 结束值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> between(String column, Object between, Object and) {
        return accept(column, TermType.btw, Arrays.asList(between, and));
    }

    /**
     * 追加NOT BETWEEN条件: column not between ? and ?
     * 
     * @param column 列名
     * @param between 起始值
     * @param and 结束值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> notBetween(String column, Object between, Object and) {
        return accept(column, TermType.nbtw, Arrays.asList(between, and));
    }

    /*------lambda-------*/

    /**
     * 追加等于条件（方法引用方式）: column = ?
     * 
     * @param column 列引用
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> is(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.eq, value);
    }

    /**
     * 追加等于条件（方法引用方式）: column = ?
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> is(MethodReferenceColumn<B> column) {
        return accept(column, TermType.eq);
    }

    /**
     * 追加LIKE条件（方法引用方式）: column like ?
     * 
     * @param column 列引用
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> like(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.like, value);
    }

    /**
     * 追加LIKE条件（方法引用方式）: column like ?
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> like(MethodReferenceColumn<B> column) {
        return accept(column, TermType.like);
    }

    /**
     * 追加LIKE条件（方法引用方式），以%结尾: column like concat(?, '%')
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> like$(MethodReferenceColumn<B> column) {
        Object value = column.get();
        if (value == null)
            return like(column.getColumn(), null);
        return accept(column.getColumn(), TermType.like, StringUtils.concat(value, "%"));
    }

    /**
     * 追加LIKE条件（方法引用方式），以%开头: column like concat('%', ?)
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> $like(MethodReferenceColumn<B> column) {
        Object value = column.get();
        if (value == null)
            return like(column.getColumn(), null);
        return accept(column.getColumn(), TermType.like, StringUtils.concat("%", value));
    }

    /**
     * 追加LIKE条件（方法引用方式），以%开头和结尾: column like concat('%', ?, '%')
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> $like$(MethodReferenceColumn<B> column) {
        Object value = column.get();
        if (value == null)
            return like(column.getColumn(), null);
        return accept(column.getColumn(), TermType.like, StringUtils.concat("%", value, "%"));
    }

    /**
     * 追加LIKE条件（方法引用方式），以%结尾: column like concat(?, '%')
     * 
     * @param column 列引用
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> like$(StaticMethodReferenceColumn<B> column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, String.valueOf(value).concat("%"));
    }

    /**
     * 追加LIKE条件（方法引用方式），以%开头: column like concat('%', ?)
     * 
     * @param column 列引用
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> $like(StaticMethodReferenceColumn<B> column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, "%".concat(String.valueOf(value)));
    }

    /**
     * 追加LIKE条件（方法引用方式），以%开头和结尾: column like concat('%', ?, '%')
     * 
     * @param column 列引用
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> $like$(StaticMethodReferenceColumn<B> column, Object value) {
        if (value == null)
            return like(column, null);
        return accept(column, TermType.like, "%".concat(String.valueOf(value)).concat("%"));
    }

    /**
     * 追加不匹配条件（方法引用方式）: column not like ?
     * 
     * @param column 列引用
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> notLike(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.nlike, value);
    }

    /**
     * 追加不匹配条件（方法引用方式）: column not like ?
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> notLike(MethodReferenceColumn<B> column) {
        return accept(column, TermType.nlike);
    }

    /**
     * 追加大于条件（方法引用方式）: column > ?
     * 
     * @param column 列引用
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> gt(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.gt, value);
    }

    /**
     * 追加大于条件（方法引用方式）: column > ?
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> gt(MethodReferenceColumn<B> column) {
        return accept(column, TermType.gt);
    }

    /**
     * 追加小于条件（方法引用方式）: column < ?
     * 
     * @param column 列引用
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> lt(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.lt, value);
    }

    /**
     * 追加小于条件（方法引用方式）: column < ?
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> lt(MethodReferenceColumn<B> column) {
        return accept(column, TermType.lt);
    }

    /**
     * 追加大于等于条件（方法引用方式）: column >= ?
     * 
     * @param column 列引用
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> gte(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.gte, value);
    }

    /**
     * 追加大于等于条件（方法引用方式）: column >= ?
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> gte(MethodReferenceColumn<B> column) {
        return accept(column, TermType.gte);
    }

    /**
     * 追加小于等于条件（方法引用方式）: column <= ?
     * 
     * @param column 列引用
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> lte(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.lte, value);
    }

    /**
     * 追加小于等于条件（方法引用方式）: column <= ?
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> lte(MethodReferenceColumn<B> column) {
        return accept(column, TermType.lte);
    }

    /**
     * 追加IN条件（方法引用方式）: column in (?)
     * 
     * @param column 列引用
     * @param values 多个条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> in(StaticMethodReferenceColumn<B> column, Object... values) {
        return accept(column, TermType.in, values);
    }

    /**
     * 追加IN条件（方法引用方式）: column in (?)
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> in(MethodReferenceColumn<B> column) {
        return accept(column, TermType.in);
    }

    /**
     * 追加IN条件（方法引用方式）: column in (?)
     * 
     * @param column 列引用
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> in(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.in, value);
    }

    /**
     * 追加IN条件（方法引用方式）: column in (?)
     * 
     * @param column 列引用
     * @param values 条件值集合
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> in(StaticMethodReferenceColumn<B> column, Collection values) {
        return accept(column, TermType.in, values);
    }

    /**
     * 追加NOT IN条件（方法引用方式）: column not in (?)
     * 
     * @param column 列引用
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> notIn(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.nin, value);
    }

    /**
     * 追加NOT IN条件（方法引用方式）: column not in (?)
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> notIn(MethodReferenceColumn<B> column) {
        return accept(column, TermType.nin);
    }

    /**
     * 追加为空条件（方法引用方式）: column = '' or column is null
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> isEmpty(StaticMethodReferenceColumn<B> column) {
        return accept(column, TermType.empty, 1);
    }

    /**
     * 追加不为空条件（方法引用方式）: column != '' and column is not null
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> notEmpty(StaticMethodReferenceColumn<B> column) {
        return accept(column, TermType.nempty, 1);
    }

    /**
     * 追加为NULL条件（方法引用方式）: column is null
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> isNull(StaticMethodReferenceColumn<B> column) {
        return accept(column, TermType.isnull, 1);
    }

    /**
     * 追加不为NULL条件（方法引用方式）: column is not null
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> notNull(StaticMethodReferenceColumn<B> column) {
        return accept(column, TermType.notnull, 1);
    }

    /**
     * 追加不等于条件（方法引用方式）: column != ?
     * 
     * @param column 列引用
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> not(StaticMethodReferenceColumn<B> column, Object value) {
        return accept(column, TermType.not, value);
    }

    /**
     * 追加不等于条件（方法引用方式）: column != ?
     * 
     * @param column 列引用
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> not(MethodReferenceColumn<B> column) {
        return accept(column, TermType.not);
    }

    /**
     * 追加BETWEEN条件（方法引用方式）: column between ? and ?
     * 
     * @param column 列引用
     * @param between 起始值函数
     * @param and 结束值函数
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> between(MethodReferenceColumn<B> column, Function<B, Object> between, Function<B, Object> and) {
        B value = column.get();
        return accept(column.getColumn(), TermType.btw, Arrays.asList(between.apply(value), and.apply(value)));
    }

    /**
     * 追加BETWEEN条件（方法引用方式）: column between ? and ?
     * 
     * @param column 列引用
     * @param between 起始值
     * @param and 结束值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> between(StaticMethodReferenceColumn<B> column, Object between, Object and) {
        return accept(column, TermType.btw, Arrays.asList(between, and));
    }

    /**
     * 追加NOT BETWEEN条件（方法引用方式）: column not between ? and ?
     * 
     * @param column 列引用
     * @param between 起始值
     * @param and 结束值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> notBetween(StaticMethodReferenceColumn<B> column, Object between, Object and) {
        return accept(column, TermType.nbtw, Arrays.asList(between, and));
    }

    /**
     * 接受条件（方法引用方式）
     * 
     * @param column 列引用
     * @param termType 条件类型
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> accept(StaticMethodReferenceColumn<B> column, String termType, Object value) {
        return getAccepter().accept(column.getColumn(), termType, value);
    }

    /**
     * 接受条件
     * 
     * @param column 列名
     * @param termType 条件类型
     * @param value 条件值
     * @return 当前嵌套条件构造器
     */
    default NestConditional<T> accept(String column, String termType, Object value) {
        return getAccepter().accept(column, termType, value);
    }

    /**
     * 接受条件（方法引用方式）
     * 
     * @param column 列引用
     * @param termType 条件类型
     * @return 当前嵌套条件构造器
     */
    default <B> NestConditional<T> accept(MethodReferenceColumn<B> column, String termType) {
        return getAccepter().accept(column.getColumn(), termType, column.get());
    }

    /**
     * 获取条件接受器
     * 
     * @return 条件接受器
     */
    Accepter<NestConditional<T>, Object> getAccepter();

    /**
     * 接受Term条件
     * 
     * @param term 条件项
     * @return 当前嵌套条件构造器
     */
    NestConditional<T> accept(Term term);
}
