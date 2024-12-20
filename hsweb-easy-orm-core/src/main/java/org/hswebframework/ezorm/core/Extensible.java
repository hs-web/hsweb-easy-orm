package org.hswebframework.ezorm.core;

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
    @JsonAnySetter
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

}
