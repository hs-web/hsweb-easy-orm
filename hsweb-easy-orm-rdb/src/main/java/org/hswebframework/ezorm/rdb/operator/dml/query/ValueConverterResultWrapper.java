package org.hswebframework.ezorm.rdb.operator.dml.query;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.DefaultColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapperContext;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;

/**
 * 查询操作器添加的列转换包装器，解码后标记上下文并委派给目标包装器。
 * 保留自定义包装器和查询事件接收已解码值的契约，已转换的列不再解码。
 *
 * @see ColumnWrapperContext#isValueConverted()
 */
@AllArgsConstructor(staticName = "of")
public class ValueConverterResultWrapper<E, R> implements ResultWrapper<E, R> {

    private final ResultWrapper<E, R> wrapper;

    private final TableOrViewMetadata metadata;

    @Override
    public E newRowInstance() {
        return wrapper.newRowInstance();
    }

    @Override
    public void beforeWrap(ResultWrapperContext context) {
        wrapper.beforeWrap(context);
    }

    @Override
    public void completedWrap() {
        wrapper.completedWrap();
    }

    @Override
    public void wrapColumn(ColumnWrapperContext<E> context) {
        RDBColumnMetadata column = metadata.getColumn(context.getColumnLabel()).orElse(null);
        if (column != null && !context.isValueConverted()) {
            Object result = column.decode(context.getResult());

            DefaultColumnWrapperContext<E> ctx = new DefaultColumnWrapperContext<>(
                    context.getColumnIndex(),
                    context.getColumnLabel(),
                    result,
                    context.getRowInstance(),
                    true
            );

            wrapper.wrapColumn(ctx);
            context.setRowInstance(ctx.getRowInstance());
        } else {
            wrapper.wrapColumn(context);
        }

    }

    @Override
    public boolean completedWrapRow(E result) {
        return wrapper.completedWrapRow(result);
    }

    @Override
    public R getResult() {
        return wrapper.getResult();
    }
}
