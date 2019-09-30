package org.hswebframework.ezorm.rdb.executor.wrapper;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.Decoder;
import org.hswebframework.ezorm.rdb.codec.JdbcResultDecoder;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Predicate;
import java.util.function.Supplier;

@SuppressWarnings("all")
public abstract class AbstractMapResultWrapper<T extends Map<String, Object>> implements ResultWrapper<T, T> {

    @Getter
    @Setter
    private Supplier<T> mapBuilder;

    @Getter
    @Setter
    private boolean wrapperNestObject = true;

    @Getter
    @Setter
    private Decoder<Object> codec = JdbcResultDecoder.INSTANCE;

    @Getter
    @Setter
    private Predicate<String> columnFilter = column -> true;

    private T currentInstance;

    @Override
    public abstract T newRowInstance();

    @Override
    public void beforeWrap(ResultWrapperContext context) {

    }

    @Override
    public void wrapColumn(ColumnWrapperContext<T> context) {
        T instance = context.getRowInstance();

        Object value = codec.decode(context.getResult());
        String column = context.getColumnLabel();

        doWrap(instance, column, value);
    }

    protected void doWrap(T instance, String column, Object value) {
        if (!columnFilter.test(column)) {
            return;
        }
        if (wrapperNestObject && column.contains(".")) {
            String[] attrs = column.split("[.]", 2);
            if (!columnFilter.test(attrs[0])) {
                return;
            }
            Object nest = instance.computeIfAbsent(attrs[0], __ -> newRowInstance());
            Map<String, Object> tmp;
            if (nest instanceof Map) {
                tmp = (Map) nest;
            } else {
                instance.put(attrs[0], tmp = newRowInstance());
                instance.put(attrs[0].concat("_old"), value);
            }

            attrs = attrs[1].split("[.]", 2);

            while (attrs.length > 1) {
                String nestColumn = attrs[0];
                if (!columnFilter.test(nestColumn)) {
                    return;
                }
                Object _nest = tmp.computeIfAbsent(nestColumn, k -> newRowInstance());
                if (_nest instanceof Map) {
                    tmp = (Map) _nest;
                } else {
                    tmp.put(nestColumn, tmp = newRowInstance());
                    tmp.put("_this_old", _nest);
                }

                attrs = attrs[1].split("[.]", 2);
            }
            tmp.put(attrs[0], value);

        } else {
            instance.put(column, value);
        }
    }

    @Override
    public boolean completedWrapRow(T result) {
        currentInstance = result;
        return true;
    }

    @Override
    public void completedWrap() {

    }

    @Override
    public T getResult() {
        return currentInstance;
    }
}
