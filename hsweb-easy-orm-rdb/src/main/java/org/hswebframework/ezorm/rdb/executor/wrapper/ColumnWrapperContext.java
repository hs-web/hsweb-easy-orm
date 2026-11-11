package org.hswebframework.ezorm.rdb.executor.wrapper;

/**
 * 一列结果的包装上下文，在当前行的包装调用中使用。
 * 列值可以是驱动原始值或查询层解码值；重建上下文的包装器应保留转换状态。
 *
 * @param <T> 当前行类型
 */
public interface ColumnWrapperContext<T> {

    int getColumnIndex();

    String getColumnLabel();

    Object getResult();

    /**
     * 包装列时用于判断 {@link #getResult()} 是否已经经过列 codec 解码。
     * 默认 false 保留直接 executor 和既有自定义上下文的原始值契约；查询转换器
     * 完成解码后设置为 true，即使解码结果与原值相等也不能再调用 codec。
     * 实现应无副作用；委派或重建上下文时应透传此状态。
     *
     * @return 是否已完成列值解码
     * @since 4.2.3
     */
    default boolean isValueConverted() {
        return false;
    }

    T getRowInstance();

    void setRowInstance(T instance);
}
