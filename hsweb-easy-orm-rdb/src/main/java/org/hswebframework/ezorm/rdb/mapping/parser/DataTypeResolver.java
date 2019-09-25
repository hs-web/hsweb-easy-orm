package org.hswebframework.ezorm.rdb.mapping.parser;

import org.hswebframework.ezorm.rdb.metadata.DataType;

import java.beans.PropertyDescriptor;

public interface DataTypeResolver {

    DataType resolve(Class entityType, PropertyDescriptor descriptor);

}
