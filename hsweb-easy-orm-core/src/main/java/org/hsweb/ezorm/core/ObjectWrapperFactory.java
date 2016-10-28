package org.hsweb.ezorm.core;


import org.hsweb.ezorm.core.meta.TableMetaData;

public interface ObjectWrapperFactory {
    <T> ObjectWrapper<T> createObjectWrapper(TableMetaData metaData);
}
