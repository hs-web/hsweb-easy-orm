package org.hswebframework.ezorm.core;

import lombok.SneakyThrows;

import java.util.Objects;
import java.util.Optional;

public interface ObjectPropertyOperator {

    Optional<Object> getProperty(Object object, String name);

    void setProperty(Object object, String name, Object value);

    /**
     * 对比两个对象
     * @param left left
     * @param right right
     * @return
     */
    @SuppressWarnings("all")
    default int compare(Object left, Object right) {
        if (Objects.equals(left, right)) {
            return 0;
        }
        if (left.getClass() == right.getClass() && left instanceof Comparable) {
            return ((Comparable) left).compareTo(right);
        }
        if (left instanceof Number && right instanceof Number) {
            return Double.compare(((Number) left).doubleValue(), ((Number) right).doubleValue());
        }
        return -1;
    }

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
