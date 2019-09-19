package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.meta.Feature;

public interface EntityManager extends Feature {

    @Override
    default String getName() {
        return MappingFeatureType.entityManager.getName();
    }

    @Override
    default String getId() {
        return MappingFeatureType.entityManager.getId();
    }

    @Override
    default MappingFeatureType getType() {
        return MappingFeatureType.entityManager;
    }

    <E> E newInstance(Class<E> type);

}
