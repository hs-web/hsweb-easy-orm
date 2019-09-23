package org.hswebframework.ezorm.spring;

import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

public interface EntityTableMetadataResolver {

    RDBTableMetadata resolve(Class<?> entityClass);

}
