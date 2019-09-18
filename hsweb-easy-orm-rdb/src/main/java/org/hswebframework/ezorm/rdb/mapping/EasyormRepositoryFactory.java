package org.hswebframework.ezorm.rdb.mapping;

public interface EasyormRepositoryFactory {

    <T, K> SyncRepository<T, K> createSyncRepository(Class<T> type);

    <T, K> ReactiveRepository<T, K> createReactiveRepository(Class<T> type);


//    <T, K, R extends SyncRepository<T, K>> R buildSyncProxy(Class<R> type);
//
//    <T, K, R extends ReactiveRepository<T, K>> R buildReactiveProxy(Class<R> type);

}
