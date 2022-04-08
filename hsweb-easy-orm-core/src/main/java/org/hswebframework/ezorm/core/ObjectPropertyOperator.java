package org.hswebframework.ezorm.core;

import lombok.SneakyThrows;

import java.util.Optional;

public interface ObjectPropertyOperator {

    Optional<Object> getProperty(Object object, String name);

    void setProperty(Object object, String name, Object value);

    @SneakyThrows
    default Optional<Class<?>> getPropertyType(Object object, String name) {
        try {
            return Optional.of(object
                                       .getClass()
                                       .getDeclaredField(name)
                                       .getType());
        } catch (Throwable e) {
            return Optional.empty();
        }
    }

    @SneakyThrows
    default Object getPropertyOrNew(Object object, String name) {
        Object value = getProperty(object, name).orElse(null);
        if (null == value) {
            Class<?> clazz = getPropertyType(object, name).orElse(null);
            if (null == clazz) {
                return null;
            }
            value = clazz.newInstance();
            setProperty(object, name, value);
        }
        return value;
    }

}
