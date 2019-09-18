package org.hswebframework.ezorm.rdb.mapping.jpa;

import org.apache.commons.beanutils.BeanUtilsBean;
import org.hswebframework.ezorm.rdb.codec.DateTimeCodec;
import org.hswebframework.ezorm.rdb.codec.NumberValueCodec;
import org.hswebframework.ezorm.rdb.mapping.DefaultEntityColumnMapping;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.JoinTable;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Date;
import java.util.Optional;

public class JpaEntityTableMetadataParserProcessor {

    private DefaultEntityColumnMapping mapping;

    private Class<?> entityType;

    private RDBTableMetadata tableMetadata;

    public JpaEntityTableMetadataParserProcessor(RDBTableMetadata tableMetadata, Class<?> entityType) {
        this.tableMetadata = tableMetadata;
        this.entityType = entityType;
        this.mapping = new DefaultEntityColumnMapping(tableMetadata, entityType);
        tableMetadata.addFeature(this.mapping);
    }

    public void process() {
        PropertyDescriptor[] descriptors = BeanUtilsBean.getInstance()
                .getPropertyUtils()
                .getPropertyDescriptors(entityType);

        //List<Runnable> lastRun = new ArrayList<>();

        for (PropertyDescriptor descriptor : descriptors) {
            Column column = getAnnotation(entityType, descriptor, Column.class);
            if (column != null) {
                handleColumnAnnotation(descriptor, column);
            }
        }

      //  lastRun.forEach(Runnable::run);
    }

    private void handleColumnAnnotation(PropertyDescriptor descriptor, Column column) {
        //另外一个表
        if (!column.table().isEmpty() && !column.table().equals(tableMetadata.getName())) {
            mapping.addMapping(column.table().concat(".").concat(column.name()), descriptor.getName());
            return;
        }
        String columnName;

        if (!column.name().isEmpty()) {
            columnName = column.name();
        } else {
            columnName = descriptor.getName();
        }
        mapping.addMapping(columnName, descriptor.getName());
        RDBColumnMetadata metadata = tableMetadata.getColumn(columnName).orElseGet(tableMetadata::newColumn);
        metadata.setName(columnName);
        metadata.setAlias(descriptor.getName());
        metadata.setJavaType(descriptor.getPropertyType());
        metadata.setLength(column.length());
        metadata.setPrecision(column.precision());
        metadata.setScale(column.scale());
        metadata.setNotNull(!column.nullable());
        metadata.setUpdatable(column.updatable());
        if (!column.columnDefinition().isEmpty()) {
            metadata.setColumnDefinition(column.columnDefinition());
        }
        tableMetadata.getDialect()
                .getJdbcType(metadata.getJavaType())
                .ifPresent(metadata::setJdbcType);

        metadata.setDataType(tableMetadata.getDialect().buildDataType(metadata));

        Optional.ofNullable(getAnnotation(entityType, descriptor, Id.class))
                .ifPresent(id -> metadata.setPrimaryKey(true));
        if(Date.class.isAssignableFrom(metadata.getJavaType())){
            metadata.setValueCodec(new DateTimeCodec("yyyy-MM-dd HH:mm:ss",metadata.getJavaType()));
        }else if(Number.class.isAssignableFrom(metadata.getJavaType())){
            metadata.setValueCodec(new NumberValueCodec(metadata.getJavaType()));
        }
        tableMetadata.addColumn(metadata);
    }


    private static <T extends Annotation> T getAnnotation(Class entityClass, PropertyDescriptor descriptor, Class<T> type) {
        T ann = null;
        try {
            Field field = entityClass.getDeclaredField(descriptor.getName());
            ann = field.getAnnotation(type);
        } catch (@SuppressWarnings("all") NoSuchFieldException ignore) {
            if (entityClass.getSuperclass() != Object.class) {
                return getAnnotation(entityClass.getSuperclass(), descriptor, type);
            }
        }
        Method read = descriptor.getReadMethod(),
                write = descriptor.getWriteMethod();
        if (null == ann && read != null) {
            ann = getAnnotation(read, type);
        }
        if (null == ann && write != null) {
            ann = getAnnotation(write, type);
        }
        return ann;
    }

    private static <T extends Annotation> T getAnnotation(Method method, Class<T> annotation) {
        T ann = method.getAnnotation(annotation);
        if (ann != null) {
            return ann;
        } else {
            Class clazz = method.getDeclaringClass();
            Class superClass = clazz.getSuperclass();
            if (superClass != null && superClass != Object.class) {
                try {
                    //父类方法
                    Method suMethod = superClass.getMethod(method.getName(), method.getParameterTypes());
                    return getAnnotation(suMethod, annotation);
                } catch (NoSuchMethodException e) {
                    return null;
                }
            }
        }
        return ann;
    }

}
