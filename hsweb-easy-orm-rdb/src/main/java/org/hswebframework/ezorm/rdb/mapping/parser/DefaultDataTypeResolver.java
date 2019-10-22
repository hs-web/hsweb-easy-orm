package org.hswebframework.ezorm.rdb.mapping.parser;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.mapping.EntityPropertyDescriptor;
import org.hswebframework.ezorm.rdb.mapping.annotation.ColumnType;
import org.hswebframework.ezorm.rdb.metadata.CustomDataType;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
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
                    RDBColumnMetadata column = descriptor.getColumn();
                    Class javaType = type.javaType()==Void.class?descriptor.getPropertyType():type.javaType();

                    if (!type.typeId().isEmpty()) {
                        return column == null ?
                                DataType.custom(type.typeId(),type.name(),type.jdbcType(),javaType )
                                :
                                column.getDialect().convertDataType(type.typeId());
                    } else if (type.type() != DataType.class) {
                        return getDataTypeInstance(type.type());
                    } else {

                        if (javaType == Void.class && null != column) {
                            return column.getDialect()
                                    .convertDataType(type.jdbcType().getName());
                        }
                        return DataType.jdbc(type.jdbcType(), javaType);
                    }
                }).orElse(null);
    }
}
