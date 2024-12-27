package org.hswebframework.ezorm.core;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;

import java.util.Map;

/**
 * 可扩展的对象,用于动态拓展实体字段属性
 *
 * @author zhouhao
 * @since 4.1.3
 */
public interface Extensible {

    /**
     * 获取所有扩展属性
     *
     * @return 扩展属性
     */
    @JsonAnyGetter
    Map<String, Object> extensions();

    /**
     * 获取扩展属性
     *
     * @param property 属性名
     * @return 属性值
     */
    default Object getExtension(String property) {
        Map<String, Object> ext = extensions();
        return ext == null ? null : ext.get(property);
    }

    /**
     * 设置扩展属性
     *
     * @param property 属性名
     * @param value    属性值
     */
    @JsonAnySetter
    void setExtension(String property, Object value);

    default void setExtension(String property, int value) {
        setExtension(property, (Object) value);
    }

    default void setExtension(String property, long value) {
        setExtension(property, (Object) value);
    }

    default void setExtension(String property, double value) {
        setExtension(property, (Object) value);
    }

    default void setExtension(String property, float value) {
        setExtension(property, (Object) value);
    }

    default void setExtension(String property, boolean value) {
        setExtension(property, (Object) value);
    }

    default void setExtension(String property, byte value) {
        setExtension(property, (Object) value);
    }

    default void setExtension(String property, char value) {
        setExtension(property, (Object) value);
    }

    default void setExtension(String property, short value) {
        setExtension(property, (Object) value);
    }


    /**
     * 方法引用方式设置扩展属性
     *
     * @param property 属性名
     * @param value    属性值
     * @param <T>      属性值类型
     */
    default <T> void setExtension(StaticMethodReferenceColumn<T> property, T value) {
        setExtension(property.getColumn(), value);
    }

    /**
     * 方法引用方式设置扩展属性
     *
     * @param property 属性名
     * @param <T>      属性值类型
     */
    default <T> void setExtension(MethodReferenceColumn<T> property) {
        setExtension(property.getColumn(), property.get());
    }

}
