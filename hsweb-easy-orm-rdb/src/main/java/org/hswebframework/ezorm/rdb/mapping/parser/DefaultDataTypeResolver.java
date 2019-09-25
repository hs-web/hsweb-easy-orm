package org.hswebframework.ezorm.rdb.mapping.parser;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.mapping.annotation.ColumnType;
import org.hswebframework.ezorm.rdb.metadata.CustomDataType;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.utils.AnnotationUtils;

import java.beans.PropertyDescriptor;

public class DefaultDataTypeResolver implements DataTypeResolver {

    public static final DefaultDataTypeResolver INSTANCE = new DefaultDataTypeResolver();

    @SneakyThrows
    protected DataType getDataTypeInstance(Class<? extends DataType> type) {
        return type.newInstance();
    }

    @Override
    public DataType resolve(Class entityType, PropertyDescriptor descriptor) {

        ColumnType type = AnnotationUtils.getAnnotation(entityType, descriptor, ColumnType.class);
        if (type != null) {
            Class javaType = type.javaType() != Void.class ? descriptor.getPropertyType() : type.javaType();

            if (!type.typeId().isEmpty()) {
                return CustomDataType.of(type.typeId(), type.typeId(), type.jdbcType(), javaType);
            } else if (type.type() != DataType.class) {
                return getDataTypeInstance(type.type());
            } else {
                return JdbcDataType.of(type.jdbcType(), javaType);
            }
        }

        return null;
    }
}
