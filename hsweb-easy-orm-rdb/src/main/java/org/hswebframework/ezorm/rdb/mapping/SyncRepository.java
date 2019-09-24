package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;

import java.util.*;

public interface SyncRepository<T, K> {

    Optional<T> findById(K primaryKey);

    List<T> findById(Collection<K> primaryKey);

    default int deleteById(K... idList) {
        return deleteById(Arrays.asList(idList));
    }

    int deleteById(Collection<K> idList);

    default SaveResult save(T... data) {
        return save(Arrays.asList(data));
    }

    SaveResult save(Collection<T> list);

    void insert(T data);

    int insertBatch(Collection<T> batch);

    SyncQuery<T> createQuery();

    SyncUpdate<T> createUpdate();

    SyncDelete createDelete();

}
