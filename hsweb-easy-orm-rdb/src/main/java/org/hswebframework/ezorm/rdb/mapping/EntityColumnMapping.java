package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

import java.util.Map;
import java.util.Optional;

public interface EntityColumnMapping extends Feature {

    @Override
    default MappingFeatureType getType() {
        return MappingFeatureType.columnPropertyMapping;
    }

    Class<?> getEntityType();

    Optional<RDBColumnMetadata> getColumnByProperty(String property);

    Optional<String> getPropertyByColumnName(String columnName);

    Optional<RDBColumnMetadata> getColumnByName(String columnName);

    Map<String,String> getColumnPropertyMapping();
}
