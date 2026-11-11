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

    /**
     * 派生当前列的上下文，保留值、行实例与已解码的列身份。
     * 派生实例的字段修改相互独立；包装器替换行实例后应回传给来源上下文。
     *
     * @param context 来源列上下文
     * @param <T> 行类型
     * @return 可独立修改的列上下文
     * @since 4.2.3
     */
    public static <T> DefaultColumnWrapperContext<T> copyOf(ColumnWrapperContext<T> context) {
        return new DefaultColumnWrapperContext<>(context.getColumnIndex(), context.getColumnLabel(),
            context.getResult(), context.getRowInstance(), context.getDecodedColumn());
    }

}
