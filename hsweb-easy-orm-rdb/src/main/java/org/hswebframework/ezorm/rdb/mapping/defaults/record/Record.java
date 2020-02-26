package org.hswebframework.ezorm.rdb.mapping.defaults.record;

import org.hswebframework.ezorm.core.MethodReferenceColumn;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;

public interface Record extends Map<String, Object> {

    default Object getObject(Object key) {
        return get(key);
    }

    Optional<Object> get(String key);

    Optional<String> getString(String key);

    Optional<Integer> getInteger(String key);

    Optional<Boolean> getBoolean(String key);

    Optional<Date> getDate(String key);

    Optional<Record> getNest(String key);

    Optional<List<Record>> getNests(String key);

    Record putValue(String key, Object value);

    default <V> Record putValue(MethodReferenceColumn<V> column) {
        return putValue(column.getColumn(), column.get());
    }

    default <T> T apply(Function<Record, T> function) {
        return function.apply(this);
    }

    default void accept(Consumer<Record> consumer) {
        consumer.accept(this);
    }

    static Record newRecord() {
        return new DefaultRecord();
    }

    static Record newRecord(Map<String, Object> map) {
        return new DefaultRecord(map);
    }
}
