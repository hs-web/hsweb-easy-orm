package org.hswebframework.ezorm.core.mapping;

import reactor.core.publisher.Mono;

import java.util.Collection;

public interface ReactiveRepository<T, K> {

    Mono<T> findById(K primaryKey);

    Mono<Void> insert(T data);

    Mono<Integer> insert(Collection<T> batch);

    ReactiveQuery<T> createQuery();

    ReactiveUpdate<T> createUpdate();

    ReactiveDelete createDelete();

}
