package org.hswebframework.ezorm.core;

import org.hswebframework.ezorm.core.param.TermType;
import reactor.function.Consumer3;
import reactor.function.Function3;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.function.*;

/**
 * 常用逻辑操作接口,通过继承或实现此接口,可提供逻辑操作支持如：when，each。
 * 常用操作方法:<br>
 * {@link LogicalOperation#when(boolean, Consumer)} <br>
 * {@link LogicalOperation#when(boolean, StaticMethodReferenceColumn, Consumer3, Object)} <br>
 * {@link LogicalOperation#each(Collection, BiConsumer)}<br>
 * {@link LogicalOperation#each(StaticMethodReferenceColumn, Collection, Consumer3, Function)} <br>
 *
 * @author zhouhao
 * @since 3.0
 */
@SuppressWarnings("unchecked")
public interface LogicalOperation<T extends LogicalOperation<?>> extends TermTypeConditionalSupport {

    /**
     * 遍历一个集合，进行条件追加
     * 例如:<br>
     * <pre>
     * query.or().each("areaId",[1,2,3],(query)->query::$like$)
     * </pre>
     * 将追加sql
     * <pre>
     * areaId like '%1%' or areaId like '%2%' or areaId like '%3%'
     * </pre>
     *
     * @param column         要追加到的列名
     * @param list           集合
     * @param accepterGetter 追加方式函数
     * @param <E>            集合中元素类型
     * @return this {@link T}
     * @see Function
     * @see Conditional
     * @see SimpleAccepter
     * @see LogicalOperation#each(StaticMethodReferenceColumn, Collection, Consumer3, Function)
     */
    @Deprecated
    default <E> T each(String column, Collection<E> list, Function<T, TermTypeConditionalSupport.SimpleAccepter<T, E>> accepterGetter) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(castSelf()).accept(column, o));
        return castSelf();
    }

    /**
     * 功能与{@link LogicalOperation#each(String, Collection, Function)}类似
     * 例如:<br>
     * <pre>
     * query.or().each("name",'like'，["%张三%","%李四%"],(query)->query::or)
     * </pre>
     * 将追加sql
     * <pre>
     * name like '%张三%' or name like '%李四%'
     * </pre>
     *
     * @param column         要追加到的列名
     * @param termType       条件类型
     * @param list           集合
     * @param accepterGetter 追加方式函数
     * @param <E>            集合中元素类型
     * @return this {@link T}
     */
    default <E> T each(String column, String termType, Collection<E> list, Function<T, TermTypeConditionalSupport.Accepter<T, E>> accepterGetter) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(castSelf()).accept(column, termType, o));
        return castSelf();
    }

    /**
     * 参照 {@link LogicalOperation#each(String, Collection, Function)}
     * 提供了一个valueMapper进行值转换如:
     * <br>
     * query.or().each("areaId",[1,2,3],(query)->query::$like$,(value)->","+value+",")<br>
     * 将追加sql<br>
     * areaId like '%,1,%' or areaId like '%,2,%' or areaId like '%,3,%'
     *
     * @param column         要追加到的列名
     * @param list           集合
     * @param accepterGetter 追加方式函数
     * @param <E>            集合中元素类型
     * @param valueMapper    值转换函数 {@link Function}
     * @return this {@link T}
     * @see Function
     * @see Conditional
     * @see SimpleAccepter
     */
    default <E, V> T each(String column,
                          Collection<E> list,
                          Function<T, TermTypeConditionalSupport.SimpleAccepter<T, V>> accepterGetter,
                          Function<E, V> valueMapper) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(castSelf()).accept(column, valueMapper.apply(o)));
        return castSelf();
    }

    /**
     * 参照 {@link LogicalOperation#each(StaticMethodReferenceColumn, Collection, Consumer3)}
     * 提供了一个valueMapper进行值转换如:
     * <br>
     * query.or().each(User::getAreaId,[1,2,3],Query::$like$,(value)->","+value+",")<br>
     * 将追加sql<br>
     * areaId like '%,1,%' or areaId like '%,2,%' or areaId like '%,3,%'
     *
     * @param column         要追加到的列名
     * @param list           集合
     * @param accepterGetter 追加方式函数
     * @param <E>            集合中元素类型
     * @param valueMapper    值转换函数 {@link Function}
     * @return this {@link T}
     * @see Function
     * @see Conditional
     * @see SimpleAccepter
     */
    default <E, V, B> T each(StaticMethodReferenceColumn<B> column,
                             Collection<E> list,
                             Consumer3<T, String, V> accepterGetter,
                             Function<E, V> valueMapper) {
        if (null != list)
            list.forEach(o -> accepterGetter.accept(castSelf(), column.getColumn(), valueMapper.apply(o)));
        return castSelf();
    }

    default <E, B> T each(StaticMethodReferenceColumn<B> column,
                          Collection<E> list,
                          Consumer3<T, String, E> accepterGetter) {
        return each(column, list, accepterGetter, Function.identity());
    }

    /**
     * <pre>
     *     query.each(list,String::toLowercase,(query,value)->query.or().is(User::AreaId,value))
     * </pre>
     *
     * @param list        集合
     * @param valueMapper 集合元素转换
     * @param consumer    查询条件构造器
     * @param <E>         集合元素类型
     * @param <V>         转换后值的类型
     * @return this
     */
    default <E, V> T each(Collection<E> list,
                          Function<E, V> valueMapper,
                          BiConsumer<T, V> consumer) {
        if (null != list)
            list.forEach(o -> consumer.accept(castSelf(), valueMapper.apply(o)));
        return castSelf();
    }

    /**
     * 功能与 {@link LogicalOperation#each(String, Collection, Function, Function)}类似，只是多了一个 termType。使用 {@link Accepter}进行操作
     *
     * @param column         要追加到的列名
     * @param termType       条件类型
     * @param list           集合
     * @param accepterGetter 自定义操作实现
     * @param valueMapper    值转换函数 {@link Function}
     * @param <E>            集合中元素类型
     * @return this {@link T}
     */
    @Deprecated
    default <E, V> T each(String column,
                          String termType,
                          Collection<E> list,
                          Function<T, TermTypeConditionalSupport.Accepter<T, V>> accepterGetter,
                          Function<E, V> valueMapper) {
        if (null != list)
            list.forEach(o -> accepterGetter.apply(castSelf()).accept(column, termType, valueMapper.apply(o)));
        return castSelf();
    }

    /**
     * 直接遍历一个集合，并提供一个消费者进行自定义操作
     * 如:
     * <pre>
     *     query.or().each([1,2,3],(query,i)->query.is('age',i))
     * </pre>
     * 生成条件如下:
     * <pre>
     *     where age= 1 or age = 2 or age = 3
     * </pre>
     *
     * @param list     遍历的集合
     * @param consumer 自定义操作实现
     * @param <E>      集合中元素的类型
     * @return this {@link T}
     * @see BiConsumer
     */
    default <E> T each(Collection<E> list, BiConsumer<T, E> consumer) {
        if (null != list)
            list.forEach(o -> consumer.accept(castSelf(), o));
        return castSelf();
    }


    /**
     * 遍历一个Map,进行条件追加
     * 例如:
     * <pre>
     *      query.each({name:"test"},(query)->query::$like$)
     *  </pre>
     * 生成条件如下:
     * <pre>
     *      where name like '%test%'
     * </pre>
     *
     * @param mapParam map参数
     * @param accepter 追加方式函数
     * @return this {@link T}
     * @see Function
     * @see Conditional
     * @see SimpleAccepter
     */
    @Deprecated
    default T each(Map<String, Object> mapParam, Function<T, TermTypeConditionalSupport.SimpleAccepter<T, Object>> accepter) {
        if (null != mapParam)
            mapParam.forEach((k, v) -> accepter.apply(castSelf()).accept(k, v));
        return castSelf();
    }

    /**
     * 遍历一个Map,进行条件追加
     * 例如:
     * <pre>
     *      query.each({name:"test"},Query::like)
     *  </pre>
     * 生成条件如下:
     * <pre>
     *      where name like '%test%'
     * </pre>
     *
     * @param mapParam map参数
     * @param accepter 追加方式函数
     * @return this {@link T}
     * @see Function
     * @see Conditional
     * @see SimpleAccepter
     */
    default <K, V> T each(Map<K, V> mapParam, Function3<T, K, V, T> accepter) {
        T self = castSelf();
        if (null != mapParam) {
            for (Map.Entry<K, V> kvEntry : mapParam.entrySet()) {
                self = accepter.apply(self, kvEntry.getKey(), kvEntry.getValue());
            }
        }
        return self;
    }

    /**
     * 功能与{@link LogicalOperation#each(Map, Function)}类似
     * 例如:
     * <pre>
     *     query.each({name:"test",age:30},'is',(query)->query::or)
     * </pre>
     * 生成条件如下:
     * <pre>
     *      where name ='test' or age = 30
     * </pre>
     *
     * @param mapParam map 参数
     * @param termType 条件类型 {@link TermType}
     * @param accepter 拼接类型函数
     * @return this {@link T}
     */
    @Deprecated
    default T each(Map<String, Object> mapParam, String termType, Function<T, TermTypeConditionalSupport.Accepter<T, Object>> accepter) {
        if (null != mapParam)
            mapParam.forEach((k, v) -> accepter.apply(castSelf()).accept(k, termType, v));
        return castSelf();
    }

    /**
     * 指定一个前置条件,当条件满足的时候,调用回调进行自定义参数<br>
     * 如: query(age>10,query->query.gt("age",10))
     *
     * @param condition 前置条件
     * @param consumer  回调
     * @return this {@link T}
     */
    default T when(boolean condition, Consumer<T> consumer) {
        if (condition) {
            consumer.accept(castSelf());
        }
        return castSelf();
    }

    /**
     * 功能与 {@link  LogicalOperation#when(boolean, Consumer)}类似
     * 通过BooleanSupplier获取条件,例如
     * <pre>query.when(()->age>10,query->query.gt("age",10));</pre>
     *
     * @return this {@link T}
     * @see Conditional#when(boolean, Consumer)
     * @see BooleanSupplier
     */
    default T when(BooleanSupplier condition, Consumer<T> consumer) {
        return when(condition.getAsBoolean(), consumer);
    }

    /**
     * 指定前置条件,列名,参数值,条件构造函数。当条件满足的时候，执行构造器添加条件.例如
     * <pre>
     *     query.when(age>10,"age",10,query->query::gt);
     * </pre>
     * 等同于
     * <pre>
     *     if(age>10)query.gt(age,10);
     * </pre>
     *
     * @param condition 前置条件
     * @param column    要查询的列名
     * @param accepter  条件构造函数
     * @param value     参数
     * @param <V>       参数类型
     * @return this {@link T}
     * @see LogicalOperation#when(boolean, String, Consumer3, Object)
     */
    @Deprecated
    default <V> T when(boolean condition, String column, V value, Function<T, TermTypeConditionalSupport.SimpleAccepter<T, V>> accepter) {
        if (condition) {
            accepter.apply(castSelf()).accept(column, value);
        }
        return castSelf();
    }

    /**
     * 指定前置条件,列名,参数值,条件构造函数。当条件满足的时候，执行构造器添加条件.例如
     * <pre>
     *     query.when(age>10,"age",Query::gt,10);
     * </pre>
     * 等同于
     * <pre>
     *     if(age>10)query.gt(age,10);
     * </pre>
     *
     * @param condition 前置条件
     * @param column    要查询的列名
     * @param accepter  条件构造函数
     * @param value     参数
     * @param <V>       参数类型
     * @return this {@link T}
     */
    default <V> T when(boolean condition, String column, Consumer3<T, String, V> accepter, V value) {
        if (condition) {
            accepter.accept(castSelf(), column, value);
        }
        return castSelf();
    }

    /**
     * 指定前置条件,列名,参数值,条件构造函数。当条件满足的时候，执行构造器添加条件.例如
     * <pre>
     *     query.when(age>10,User::getAage,Query::gt,10);
     * </pre>
     * 等同于
     * <pre>
     *     if(age>10)query.gt(age,10);
     * </pre>
     *
     * @param condition 前置条件
     * @param column    要查询的列名
     * @param accepter  条件构造函数
     * @param value     参数
     * @param <V>       参数类型
     * @return this {@link T}
     */
    default <V, B> T when(boolean condition, StaticMethodReferenceColumn<B> column, Consumer3<T, String, V> accepter, V value) {
        return when(condition, column.getColumn(), accepter, value);
    }

    /**
     * 指定前置条件,列名,参数值,条件构造函数。当条件满足的时候，执行构造器添加条件.例如
     * <pre>
     *     query.when(age>10,Query::gt,user::getAage);
     * </pre>
     * 等同于
     * <pre>
     *     if(age>10)query.gt(age,10);
     * </pre>
     *
     * @param condition 前置条件
     * @param column    要查询的列名
     * @param accepter  条件构造函数
     * @param <V>       参数类型
     * @return this {@link T}
     */
    default <V> T when(boolean condition, Consumer3<T, String, V> accepter, MethodReferenceColumn<V> column) {

        return when(condition, column.getColumn(), accepter, column.get());
    }

    /**
     * 指定列名，参数值，条件判断函数，条件构造函数进行条件添加.例如
     * <pre>
     *     query.when("age",10,value->value>10,query->query::gt)
     * </pre>
     *
     * @param column    列名
     * @param value     参数
     * @param <V>       参数类型
     * @param condition 条件判断函数
     * @param accepter  条件构造函数
     * @return this {@link T}
     * @see Conditional#when(boolean, String, Object, Function)
     */
    default <V> T when(String column, V value, Function<V, Boolean> condition, Function<T, TermTypeConditionalSupport.SimpleAccepter<T, V>> accepter) {
        return when(condition.apply(value), column, value, accepter);
    }

    /**
     * 功能与{@link Conditional#when(boolean, String, Object, Function)} 类似,可自定义termType 例如
     * <pre>
     *     query.when(true,"age","like",10,query->query::or)
     * </pre>
     *
     * @param condition 条件
     * @param column    列名
     * @param termType  条件类型
     * @param value     参数
     * @param <V>       参数类型
     * @param accepter  条件构造函数
     * @return this {@link T}
     * @see Conditional#when(boolean, String, Object, Function)
     */
    @Deprecated
    default <V> T when(boolean condition, String column, String termType, V value, Function<T, TermTypeConditionalSupport.Accepter<T, V>> accepter) {
        if (condition) {
            accepter.apply(castSelf()).accept(column, termType, value);
        }
        return castSelf();
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
     * @param <V>       参数类型
     * @return this {@link T}
     * @see Conditional#when(boolean, String, Object, Function)
     * @see TermType
     */
    @Deprecated
    default <V> T when(String column, String termType, V value, Function<V, Boolean> condition, Function<T, TermTypeConditionalSupport.Accepter<T, V>> accepter) {
        return when(condition.apply(value), column, termType, value, accepter);
    }

    @SuppressWarnings("all")
    default <V> T when(Optional<V> value, BiConsumer<T, V> consumer) {
        value.ifPresent(v -> consumer.accept(castSelf(), v));
        return castSelf();
    }

    default <R> R as(Function<T, R> function) {
        return function.apply(castSelf());
    }

    default T accept(Consumer<T> consumer) {
        consumer.accept(castSelf());
        return castSelf();
    }

    default <V> T accept(V v, BiConsumer<T, V> consumer) {
        consumer.accept(castSelf(), v);
        return castSelf();
    }

    default <V> T accept(MethodReferenceColumn<V> column, BiConsumer<T, V> consumer) {
        V v = column.get();
        if (v != null) {
            consumer.accept(castSelf(), v);
        }
        return castSelf();
    }

    default T castSelf() {
        return (T) this;
    }
}
