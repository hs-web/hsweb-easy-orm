package org.hswebframework.ezorm.core.mapping;

import java.util.Collection;

public interface SyncRepository<T, K> {

    T findById(K primaryKey);

    void insert(T data);

    int insert(Collection<T> batch);

    SyncQuery<T> createQuery();

    SyncUpdate<T> createUpdate();

    SyncDelete createDelete();

}
