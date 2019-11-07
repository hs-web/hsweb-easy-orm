package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.function.Tuple2;

import java.util.Collection;

public interface ReactiveRepository<T, K> {

    Mono<T> newInstance();

    default Mono<T> findById(K key) {
        return findById(Mono.just(key));
    }

    default Flux<T> findById(Collection<K> key) {
        return findById(Flux.fromIterable(key));
    }

    Mono<T> findById(Mono<K> key);

    Flux<T> findById(Flux<K> key);

    default Mono<Integer> deleteById(K key) {
        return deleteById(Mono.just(key));
    }

    default Mono<Integer> deleteById(Collection<K> key) {
        return deleteById(Flux.fromIterable(key));
    }

    Mono<Integer> deleteById(Publisher<K> key);

    default Mono<SaveResult> save(T data) {
        return save(Mono.just(data));
    }

    default Mono<SaveResult> save(Collection<T> data) {
        return save(Flux.fromIterable(data));
    }

    Mono<SaveResult> save(Publisher<T> data);

    default Mono<Integer> updateById(K id, T data) {
        return updateById(id, Mono.just(data));
    }

    Mono<Integer> updateById(K id, Mono<T> data);

    Mono<Integer> insert(Publisher<T> data);

    default Mono<Integer> insert(T data) {
        return insert(Mono.just(data));
    }

    default Mono<Integer> insert(Collection<T> data) {
        return insert(Flux.fromIterable(data));
    }

    default Mono<Integer> insertBatch(Collection<T> data) {
        return insertBatch(Mono.just(data));
    }

    Mono<Integer> insertBatch(Publisher<? extends Collection<T>> data);

    ReactiveQuery<T> createQuery();

    ReactiveUpdate<T> createUpdate();

    ReactiveDelete createDelete();

}
