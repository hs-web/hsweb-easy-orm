package org.hswebframework.ezorm.rdb.executor.wrapper;


/**
 * 查询结果包装器
 *
 * @param <E> 结果集元素
 * @param <R> 最终返回结果
 * @see ResultWrappers
 */
public interface ResultWrapper<E, R> {

    /**
     * @return 一行对应的对象实例
     */
    E newRowInstance();

    /**
     * 在开始执行包装前执行此方法
     *
     * @param context 包装器上下文
     */
    default void beforeWrap(ResultWrapperContext context) {
    }

    /**
     * 包装一列时执行
     *
     * @param context 列包装器上下文
     */
    void wrapColumn(ColumnWrapperContext<E> context);

    /**
     * 完成一行的包装时执行
     *
     * @param result 行结果对象
     * @return 是否继续下一行
     */
    boolean completedWrapRow(E result);

    /**
     * 全部包装完成时执行此方法
     */
    default void completedWrap() {
    }

    /**
     * @return 当前包装结果
     */
    R getResult();
}
