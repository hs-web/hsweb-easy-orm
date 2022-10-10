package org.hswebframework.ezorm.rdb.operator.dml.query;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.DefaultColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapperContext;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;

import java.util.Objects;

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
        if (column != null) {
            Object origin = context.getResult();
            Object result = column.decode(origin);

            //值相同则不需要转换
            if (Objects.equals(origin, result)) {
                wrapper.wrapColumn(context);
                return;
            }

            DefaultColumnWrapperContext<E> ctx = new DefaultColumnWrapperContext<>(
                    context.getColumnIndex(),
                    context.getColumnLabel(),
                    result,
                    context.getRowInstance()
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
