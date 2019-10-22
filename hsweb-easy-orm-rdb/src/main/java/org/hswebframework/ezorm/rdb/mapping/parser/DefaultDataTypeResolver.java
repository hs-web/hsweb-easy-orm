package org.hswebframework.ezorm.rdb.mapping.parser;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.mapping.EntityPropertyDescriptor;
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
    public DataType resolve(EntityPropertyDescriptor descriptor) {

        return descriptor.findAnnotation(ColumnType.class)
                .map(type -> {

                    if (!type.typeId().isEmpty()) {
                        return descriptor.getColumn()
                                .getDialect()
                                .convertDataType(type.typeId());
                    } else if (type.type() != DataType.class) {
                        return getDataTypeInstance(type.type());
                    } else {
                        Class javaType = type.javaType();
                        if (javaType == Void.class) {
                            return descriptor.getColumn()
                                    .getDialect()
                                    .convertDataType(type.jdbcType().getName());
                        }
                        return DataType.jdbc(type.jdbcType(), javaType);
                    }
                }).orElse(null);
    }
}
