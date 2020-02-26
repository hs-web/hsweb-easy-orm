package org.hswebframework.ezorm.rdb.mapping.jpa;

import lombok.AllArgsConstructor;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.mapping.EntityPropertyDescriptor;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.utils.AnnotationUtils;
import org.hswebframework.ezorm.rdb.utils.PropertiesUtils;

import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@AllArgsConstructor(staticName = "of")
public class SimpleEntityPropertyDescriptor implements EntityPropertyDescriptor {

    private Class entityType;

    private String propertyName;

    private Class propertyType;

    private RDBColumnMetadata column;

    private PropertyDescriptor descriptor;

    public static EntityPropertyDescriptor of(PropertyDescriptor descriptor, RDBColumnMetadata column) {
        return SimpleEntityPropertyDescriptor.of(
                descriptor.getReadMethod().getDeclaringClass(),
                descriptor.getName(),
                descriptor.getPropertyType(),
                column,
                descriptor
        );
    }

    @Override
    public String getPropertyName() {
        return propertyName;
    }

    @Override
    @SneakyThrows
    public Field getField() {
        return PropertiesUtils.getPropertyField(entityType, descriptor.getName()).orElseThrow(() -> new NoSuchFieldException("no such field " + propertyName + " in " + entityType));
    }

    @Override
    public Class getPropertyType() {
        return propertyType;
    }

    @Override
    public RDBColumnMetadata getColumn() {
        return column;
    }

    @Override
    public <T extends Annotation> Optional<T> findAnnotation(Class<T> tClass) {

        return Optional.ofNullable(AnnotationUtils.getAnnotation(entityType, descriptor, tClass));
    }

    @Override
    public Set<Annotation> getAnnotations() {
        return AnnotationUtils.getAnnotations(entityType, descriptor);
    }
}
