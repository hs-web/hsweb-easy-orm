package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.function.Tuple2;

import java.util.Collection;

public interface ReactiveRepository<T, K> {

    Mono<T> newInstance();

    Mono<T> findById(Mono<K> key);

    Flux<T> findById(Flux<K> key);

    Mono<Integer> deleteById(Publisher<K> key);

    Mono<SaveResult> save(Publisher<T> data);

    Mono<Integer> updateById(K id, Mono<T> data);

    Mono<Integer> insert(Publisher<T> data);

    Mono<Integer> insertBatch(Publisher<? extends Collection<T>> data);

    ReactiveQuery<T> createQuery();

    ReactiveUpdate<T> createUpdate();

    ReactiveDelete createDelete();

}
