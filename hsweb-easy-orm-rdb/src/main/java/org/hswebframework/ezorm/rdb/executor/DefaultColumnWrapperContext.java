package org.hswebframework.ezorm.rdb.executor;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

/**
 * executor 和查询包装器创建的单列上下文，包含当前值、行实例及解码状态。
 * 四参数构造器接收驱动原始值，保留直接 executor 的调用契约。
 */
@Getter
@Setter
@AllArgsConstructor
public class DefaultColumnWrapperContext<T> implements ColumnWrapperContext<T> {

    private int columnIndex;

    private String columnLabel;

    private Object result;

    private T rowInstance;

    private RDBColumnMetadata decodedColumn;

    public DefaultColumnWrapperContext(int columnIndex, String columnLabel, Object result, T rowInstance) {
        this(columnIndex, columnLabel, result, rowInstance, null);
    }

}
