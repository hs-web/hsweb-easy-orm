package org.hswebframework.ezorm.rdb.mapping;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.utils.StringUtils;

@Getter
@AllArgsConstructor
public enum MappingFeatureType implements FeatureType {

    /**
     * @see EntityColumnMapping
     */
    columnPropertyMapping("列与属性映射关系"),

    /**
     * @see EntityManager
     */
    entityManager("实体类管理器"),

    /**
     * @see EntityPropertyDescriptor
     */
    propertyDescriptor("属性描述器")
    ;

    private final String name;

    @Override
    public String getId() {
        return name();
    }

    public String createFeatureId(Class<?> type) {
        return StringUtils.concat(getId(),":",type.getName());
    }
}
