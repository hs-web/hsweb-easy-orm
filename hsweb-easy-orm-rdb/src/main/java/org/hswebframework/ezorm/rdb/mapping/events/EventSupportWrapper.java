package org.hswebframework.ezorm.rdb.mapping.events;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;

import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.*;

@AllArgsConstructor
public class EventSupportWrapper<E, R> implements ResultWrapper<E, R> {

    private TableOrViewMetadata metadata;

    private ResultWrapper<E, R> wrapper;

    private ContextKeyValue<?>[] defaultKeyValues;

    public static <E, R> EventSupportWrapper<E, R> eventWrapper(TableOrViewMetadata metadata,
                                                                ResultWrapper<E, R> wrapper,
                                                                ContextKeyValue<?>... contextKeyValues) {
        return new EventSupportWrapper<>(metadata, wrapper, contextKeyValues);
    }

    @Override
    public E newRowInstance() {
        return wrapper.newRowInstance();
    }

    @Override
    public void wrapColumn(ColumnWrapperContext<E> context) {
        wrapper.wrapColumn(context);
        metadata.fireEvent(MappingEventTypes.select_wrapper_column, ctx -> ctx.set(columnWrapperContext(context)).set(defaultKeyValues));
    }

    @Override
    public boolean completedWrapRow(E result) {
        boolean val = wrapper.completedWrapRow(result);
        metadata.fireEvent(MappingEventTypes.select_wrapper_done, ctx -> ctx.set(instance(result)).set(defaultKeyValues));
        return val;
    }

    @Override
    public R getResult() {
        R result = wrapper.getResult();
        metadata.fireEvent(MappingEventTypes.select_done, ctx -> ctx.set(result(result)).set(defaultKeyValues));
        return result;
    }
}
