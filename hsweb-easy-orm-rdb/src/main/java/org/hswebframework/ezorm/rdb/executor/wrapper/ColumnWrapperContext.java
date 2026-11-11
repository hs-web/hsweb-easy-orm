package org.hswebframework.ezorm.rdb.executor.wrapper;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

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
     * 包装列时用于判断 {@link #getResult()} 已经经过哪个列映射解码。
     * 默认 null 表示没有已完成的解码，保留直接 executor 和既有自定义上下文的契约。
     * 仅当目标列与返回值为同一实例时才跳过解码；不同列可能共享 codec，
     * 但仍有各自的元数据配置和转换职责。有效 codec 返回原值时也应记录该列。
     * 实现应无副作用；委派或重建上下文时应透传该列实例，不复制列元数据。
     *
     * @return 已完成解码的列映射，没有则返回 null
     * @since 4.2.3
     */
    default RDBColumnMetadata getDecodedColumn() {
        return null;
    }

    T getRowInstance();

    void setRowInstance(T instance);
}
