package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public interface EntityPropertyDescriptor extends Feature {

    String ID_VALUE = "entityPropertyDescriptor";

    FeatureId<EntityPropertyDescriptor> ID = FeatureId.of(ID_VALUE);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return getType().getName();
    }

    @Override
    default FeatureType getType() {
        return MappingFeatureType.propertyDescriptor;
    }

    String getPropertyName();

    Class getPropertyType();

    Field getField();

    RDBColumnMetadata getColumn();

    <T extends Annotation> Optional<T> findAnnotation(Class<T> aClass);

    Set<Annotation> getAnnotations();

}
