package org.hswebframework.ezorm.rdb.mapping;

import org.reactivestreams.Publisher;
import reactor.core.publisher.Mono;

import java.util.Collection;

public interface ReactiveRepository<T, K> {

    Mono<T> findById(Mono<K> key);

    Mono<Integer> insert(Publisher<T> data);

    Mono<Integer> insertBatch(Publisher<Collection<T>> data);

    ReactiveQuery<T> createQuery();

    ReactiveUpdate<T> createUpdate();

    ReactiveDelete createDelete();

}
