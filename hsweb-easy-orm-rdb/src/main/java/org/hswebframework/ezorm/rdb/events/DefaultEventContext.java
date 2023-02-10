package org.hswebframework.ezorm.rdb.events;

import org.hswebframework.ezorm.core.CastUtil;

import java.util.HashMap;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

class DefaultEventContext extends ConcurrentHashMap<String, Object> implements EventContext {

    @Override
    public Object get(String key) {
        return get((Object) key);
    }

    @Override
    public <T> Optional<T> get(ContextKey<T> key) {
        return Optional.ofNullable(get(key.getKey()))
                .map(CastUtil::cast);
    }

    @Override
    public <T> EventContext set(ContextKey<T> key, T value) {
        put(key.getKey(), value);
        return this;
    }

    @Override
    public <T> EventContext set(String key, T value) {
        put(key, value);
        return this;
    }

    @Override
    public EventContext set(ContextKeyValue<?>... keyValue) {
        for (ContextKeyValue<?> contextKeyValue : keyValue) {
            put(contextKeyValue.getKey(),contextKeyValue.getValue());
        }
        return this;
    }

}
