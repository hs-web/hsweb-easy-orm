package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.core.param.QueryParam;

import java.util.Arrays;
import java.util.function.BiFunction;

/**
 * DSL动态更新条件
 *
 * @param <E>  实体类型
 * @param <ME> 实现此接口的类型
 * @author zhouhao
 * @since 4.0.0
 */
@SuppressWarnings("all")
public interface DSLUpdate<E, ME extends DSLUpdate> extends Conditional<ME> {

    /**
     * 指定只需要更新的属性(列)
     *
     * @param properties 属性列表
     * @return this
     */
    ME includes(String... properties);

    /**
     * 指定不更新的属性(列)
     *
     * @param properties 属性列表
     * @return this
     */
    ME excludes(String... properties);

    /**
     * 使用getter静态方法引用来指定需要更新的属性(列),
     * 此方法可能会被IDE警告.
     * <pre>
     *     createUpdate()
     *      .set(user)
     *      .includes(User::getName,User::getState)
     *      .where(User::getId,id)
     *      .execute();
     * </pre>
     *
     * @param columns 静态方法引用
     * @return this
     */
    default ME includes(StaticMethodReferenceColumn<E>... columns) {
        return includes(Arrays.stream(columns)
                              .map(StaticMethodReferenceColumn::getColumn)
                              .toArray(String[]::new));
    }

    /**
     * 使用getter静态方法引用来指定不需要更新的属性(列),
     * 此方法可能会被IDE警告.
     * <pre>
     *     createUpdate()
     *      .set(user)
     *      .excludes(User::getName,User::getState)
     *      .where(User::getId,id)
     *      .execute();
     * </pre>
     *
     * @param columns 静态方法引用
     * @return this
     */
    default ME excludes(StaticMethodReferenceColumn<E>... columns) {
        return excludes(Arrays.stream(columns)
                              .map(StaticMethodReferenceColumn::getColumn)
                              .toArray(String[]::new));
    }

    /**
     * 使用getter方法引用来指定需要更新的属性(列),
     * 此方法可能会被IDE警告.
     * <pre>
     *     createUpdate()
     *      .set(user)
     *      .includes(user::getName,user::getState)
     *      .where(User::getId,id)
     *      .execute();
     * </pre>
     *
     * @param columns 静态方法引用
     * @return this
     */
    default ME includes(MethodReferenceColumn<E>... columns) {
        return includes(Arrays.stream(columns)
                              .map(MethodReferenceColumn::getColumn)
                              .toArray(String[]::new));
    }

    /**
     * 使用getter方法引用来指定不需要更新的属性(列),
     * 此方法可能会被IDE警告.
     * <pre>
     *     createUpdate()
     *      .set(user)
     *      .excludes(user::getName,user::getState)
     *      .where(User::getId,id)
     *      .execute();
     * </pre>
     *
     * @param columns 静态方法引用
     * @return this
     */
    default ME excludes(MethodReferenceColumn<E>... columns) {
        return excludes(Arrays.stream(columns)
                              .map(MethodReferenceColumn::getColumn)
                              .toArray(String[]::new));
    }

    /**
     * 按实体类更新,为<code>null</code>的属性会被忽略.
     *
     * @param entity 实体类
     * @return this
     */
    ME set(E entity);

    /**
     * 设置属性(列)值
     *
     * @param column 属性或者列名
     * @param value  值
     * @return this
     */
    ME set(String column, Object value);

    /**
     * 设置属性(列)的值为<code>null</code>
     *
     * @param column 属性或者列明
     * @return this
     */
    ME setNull(String column);

    /**
     * 使用getter方法引用来设置属性值
     * <pre>
     * //对应sql update table set name = ? where id = ?
     * createUpdate()
     *  .set(user::getName)
     *  .where(user::getId)
     *  .execute();
     * </pre>
     *
     * @param columnAndValue 方法引用
     * @param <R>
     * @return this
     */
    default <R> ME set(MethodReferenceColumn<R> columnAndValue) {
        return set(columnAndValue.getColumn(), columnAndValue.get());
    }

    /**
     * 使用getter静态方法引用来设置属性值
     * <pre>
     *
     * createUpdate()
     *  .set(User::getName,name)
     *  .where(User::getId,id)
     *  .execute();
     * </pre>
     *
     * @param column 静态方法引用
     * @param value  值
     * @return this
     */
    default ME set(StaticMethodReferenceColumn<E> column, Object value) {
        return set(column.getColumn(), value);
    }

    /**
     * 使用getter静态方法引用来设置null值
     * <pre>
     * createUpdate()
     *  .setNull(User::getName)
     *  .where(User::getId,id)
     *  .execute();
     * </pre>
     *
     * @param column 列
     * @return this
     */
    default ME setNull(StaticMethodReferenceColumn<E> column) {
        return setNull(column.getColumn());
    }

    /**
     * 使用getter方法引用来设置null值
     * <pre>
     * createUpdate()
     *  .setNull(user::getName)
     *  .where(User::getId,id)
     *  .execute();
     * </pre>
     *
     * @param column 列
     * @return this
     */
    default ME setNull(MethodReferenceColumn<E> column) {
        return setNull(column.getColumn());
    }

    /**
     * 将更新条件转为查询条件,通常用于根据更新来查询可能被更新的数据
     *
     * @return 动态查询条件
     */
    public QueryParam toQueryParam();

}
