package org.hswebframework.ezorm.rdb.mapping;

import java.util.Collection;
import java.util.Optional;

public interface SyncRepository<T, K> {

    Optional<T> findById(K primaryKey);

    void insert(T data);

    int insertBatch(Collection<T> batch);

    SyncQuery<T> createQuery();

    SyncUpdate<T> createUpdate();

    SyncDelete createDelete();

}
