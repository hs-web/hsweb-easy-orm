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
    propertyDescriptor("属性描述器"),

    /**
     * @see ReactiveQueryResultTransformer
     */
    reactiveQueryResultTransformer("响应式实体查询结果变换"),

    /**
     * @see ReactiveCrudExecutionInterceptor
     */
    reactiveCrudExecutionInterceptor("响应式CRUD执行拦截")
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
