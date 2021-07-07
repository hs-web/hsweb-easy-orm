package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.rdb.operator.dml.SortOrderSupplier;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;

import java.util.Arrays;
import java.util.Map;

/**
 * 动态DSL查询接口,用于通过DSL方式构造动态查询条件
 *
 * @param <ME> 实现此接口的类型
 * @author zhouhao
 * @see QueryParam
 * @since 4.0.0
 */
@SuppressWarnings("all")
public interface DSLQuery<ME extends DSLQuery<?>> extends Conditional<ME> {

    /**
     * 查询指定的属性(列)
     *
     * @param columns 属性(列)
     * @return this
     */
    ME select(String... columns);

    /**
     * 不查询指定的属性(列)
     *
     * @param columns 属性(列)
     * @return this
     */
    ME selectExcludes(String... columns);

    /**
     * 使用getter静态方法引用来指定查询的属性
     * <pre>
     * createQuery()
     * .select(User::getName)
     * .fetch()
     * </pre>
     *
     * @param column 列
     * @param <T>    type
     * @return this
     */
    <T> ME select(StaticMethodReferenceColumn<T>... column);

    /**
     * 使用getter方法引用来指定查询的属性
     * <pre>
     * createQuery()
     * .select(user::getName)
     * .fetch()
     * </pre>
     *
     * @param column 列
     * @param <T>    type
     * @return this
     */
    <T> ME select(MethodReferenceColumn<T>... column);

    /**
     * 使用getter静态方法引用来指定不查询的属性
     * <pre>
     * createQuery()
     * .selectExcludes(User::getName)
     * .fetch()
     * </pre>
     *
     * @param column 列
     * @param <T>    type
     * @return this
     */
    <T> ME selectExcludes(StaticMethodReferenceColumn<T>... column);

    /**
     * 使用getter方法引用来指定不查询的属性
     * <pre>
     * createQuery()
     * .selectExcludes(user::getName)
     * .fetch()
     * </pre>
     *
     * @param column 列
     * @param <T>    type
     * @return this
     */
    <T> ME selectExcludes(MethodReferenceColumn<T>... column);

    /**
     * 指定分页条件
     *
     * @param pageIndex 页码,从0开始.
     * @param pageSize  每页数量
     * @return this
     */
    ME paging(int pageIndex, int pageSize);

    /**
     * 指定排序,支持多列排序
     *
     * @param orders 排序
     * @return this
     * @see SortOrder
     * @see SortOrder#asc(String)
     * @see SortOrder#desc(String)
     */
    ME orderBy(SortOrder... orders);

    /**
     * 指定排序,支持多列排序
     *
     * @param orders 排序
     * @return this
     * @see org.hswebframework.ezorm.rdb.operator.dml.query.Orders#asc(String)
     * @see org.hswebframework.ezorm.rdb.operator.dml.query.Orders#desc(String)
     */
    ME orderBy(SortOrderSupplier... orders);

    /**
     * 直接设置动态查询条件,调用此方法后,通过上述方法调用的条件都会被覆盖.
     *
     * @param param 条件
     * @return this
     */
    ME setParam(QueryParam param);

    /**
     * select * from xx for update
     * @return this
     * @since 4.0.11
     */
    ME forUpdate();

    /**
     * 设置上下文
     * @param context ctx
     * @return this
     * @since 4.0.11
     */
    ME context(Map<String,Object> context);

    /**
     * 设置上下文
     * @param context ctx
     * @return this
     * @since 4.0.11
     */
    ME context(String key,Object value);
}
