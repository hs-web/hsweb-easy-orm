package org.hsweb.ezorm.meta.expand;


import org.hsweb.ezorm.meta.TableMetaData;

public interface ObjectWrapperFactory {
    <T> ObjectWrapper<T> createObjectWrapper(TableMetaData metaData);
}
