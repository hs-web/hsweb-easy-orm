package org.hswebframework.ezorm.core;


import org.hswebframework.ezorm.core.meta.TableMetaData;

public interface ObjectWrapperFactory {
    <T> ObjectWrapper<T> createObjectWrapper(TableMetaData metaData);
}
