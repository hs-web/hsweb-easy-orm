package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.function.Tuple2;

import java.util.Collection;

/**
 * 响应式仓库接口
 *
 * @param <T> 实体类型
 * @param <K> 主键类型
 */
public interface ReactiveRepository<T, K> {

    Mono<T> newInstance();

    Mono<T> findById(Mono<K> key);

    Flux<T> findById(Flux<K> key);

    default Mono<T> findById(K key) {
        return findById(Mono.just(key));
    }

    default Flux<T> findById(Collection<K> key) {
        return findById(Flux.fromIterable(key));
    }

    Mono<Integer> deleteById(Publisher<K> key);

    default Mono<Integer> deleteById(K key) {
        return deleteById(Mono.just(key));
    }

    default Mono<Integer> deleteById(Collection<K> key) {
        return deleteById(Flux.fromIterable(key));
    }

    Mono<SaveResult> save(Publisher<T> data);

    default Mono<SaveResult> save(T data) {
        return save(Mono.just(data));
    }

    default Mono<SaveResult> save(Collection<T> data) {
        return save(Flux.fromIterable(data));
    }

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

    /**
     * DSL方式动态查询
     *
     * @return 动态查询器
     */
    ReactiveQuery<T> createQuery();

    /**
     * DSL动态更新
     *
     * @return 更新器
     */
    ReactiveUpdate<T> createUpdate();

    /**
     * DSL动态删除
     *
     * @return 删除器
     */
    ReactiveDelete createDelete();

}
