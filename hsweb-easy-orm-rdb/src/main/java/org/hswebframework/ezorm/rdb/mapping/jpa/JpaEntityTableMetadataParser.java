package org.hswebframework.ezorm.rdb.mapping.jpa;


import org.apache.commons.beanutils.BeanUtilsBean;
import org.hswebframework.ezorm.rdb.mapping.EntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.utils.ClassUtils;

import javax.persistence.*;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @see javax.persistence.Column
 * @see javax.persistence.JoinTable
 * @see javax.persistence.CollectionTable
 */
public class JpaEntityTableMetadataParser implements EntityTableMetadataParser {

    private RDBDatabaseMetadata databaseMetadata;

    private Map<Class<?>, RDBTableMetadata> cache = new ConcurrentHashMap<>();

    public Optional<RDBTableMetadata> parseTable(Class<?> entityType) {

        Table table = ClassUtils.getAnnotation(entityType, Table.class);
        if (table == null) {
            return Optional.empty();
        }
        RDBSchemaMetadata schema = databaseMetadata.getSchema(table.schema())
                .orElseGet(databaseMetadata::getCurrentSchema);


        RDBTableMetadata tableMetadata = schema.newTable(table.name());
        tableMetadata.setAlias(entityType.getSimpleName());

        PropertyDescriptor[] descriptors = BeanUtilsBean.getInstance()
                .getPropertyUtils()
                .getPropertyDescriptors(entityType);

        List<Runnable> lastRun = new ArrayList<>();

        for (PropertyDescriptor descriptor : descriptors) {
            Column column = getAnnotation(entityType, descriptor, Column.class);
            if (column != null) {
                handleColumnAnnotation(tableMetadata, descriptor, column);
            }
            JoinTable joinTable = getAnnotation(entityType, descriptor, JoinTable.class);
            lastRun.add(() -> handleJoinTable(tableMetadata, descriptor, joinTable));

        }

        lastRun.forEach(Runnable::run);

        return Optional.of(tableMetadata);

    }

    private void handleJoinTable(RDBTableMetadata tableMetadata, PropertyDescriptor descriptor, JoinTable joinTable) {
        ForeignKeyBuilder.ForeignKeyBuilderBuilder builder = ForeignKeyBuilder.builder();


        for (JoinColumn joinColumn : joinTable.joinColumns()) {

        }
        tableMetadata.addForeignKey(builder.build());

    }

    private void handleColumnAnnotation(RDBTableMetadata tableMetadata, PropertyDescriptor descriptor, Column column) {
        //另外一个表
        if (!column.table().isEmpty() && !column.table().equals(tableMetadata.getName())) {
            return;
        }
        String columnName;

        if (!column.name().isEmpty()) {
            columnName = column.name();
        } else {
            columnName = descriptor.getName();
        }
        RDBColumnMetadata metadata = tableMetadata.getColumn(columnName).orElseGet(tableMetadata::newColumn);

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
        metadata.getDialect()
                .getJdbcType(metadata.getJavaType())
                .ifPresent(metadata::setJdbcType);
        metadata.setDataType(metadata.getDialect().buildDataType(metadata));

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
            ann = ClassUtils.getAnnotation(read, type);
        }
        if (null == ann && write != null) {
            ann = ClassUtils.getAnnotation(write, type);
        }
        return ann;
    }


}
