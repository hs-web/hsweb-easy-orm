package org.hswebframework.ezorm.rdb.executor;

import java.util.*;

public class DefaultRecord extends HashMap<String, Object> implements Record {

    public DefaultRecord() {

    }
    public DefaultRecord(Map<String, Object> map) {
        super(map);
    }

    @Override
    public Optional<Object> get(String key) {
        return Optional.ofNullable(this.get((Object) key));
    }

    @Override
    public Optional<String> getString(String key) {
        return get(key)
                .map(String::valueOf);
    }

    @Override
    public Optional<Integer> getInteger(String key) {
        return get(key)
                .map(Number.class::cast)
                .map(Number::intValue);
    }

    @Override
    public Optional<Boolean> getBoolean(String key) {
        return get(key)
                .map(val -> Boolean.TRUE.equals(val) || val.equals(1));
    }

    @Override
    public Optional<Date> getDate(String key) {
        return get(key)
                .map(Date.class::cast);
    }

    @Override
    public Optional<Record> getNest(String key) {
        return get(key)
                .map(val -> {
                    if (val instanceof Record) {
                        return ((Record) val);
                    }
                    if (val instanceof Map) {
                        return new DefaultRecord(((Map) val));
                    }
                    throw new UnsupportedOperationException("value [" + val + "] is not nest property");
                });
    }

    @Override
    @SuppressWarnings("all")
    public Optional<List<Record>> getNests(String key) {
        return get(key)
                .map(val -> {
                    if (val instanceof Record) {
                        return Collections.singletonList(((Record) val));
                    }
                    if (val instanceof Map) {
                        return Collections.singletonList(new DefaultRecord(((Map) val)));
                    }
                    if (val instanceof Collection) {
                        return new ArrayList<>(((Collection) val));
                    }
                    throw new UnsupportedOperationException("value [" + val + "] is not nest property");
                });
    }
}
