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
 * @author zhouhao
 * @since 4.0.0
 */
public interface ReactiveRepository<T, K> {

    /**
     * 创建实体类实例
     *
     * @return 实体类实例
     */
    Mono<T> newInstance();

    /**
     * 根据异步ID查询
     *
     * @param key key
     * @return 异步查询结果
     */
    Mono<T> findById(Mono<K> key);

    /**
     * 根据异步ID查询
     *
     * @param key key
     * @return 异步查询结果
     */
    Flux<T> findById(Flux<K> key);

    /**
     * 根据ID查询
     *
     * @param key key
     * @return 异步查询结果
     */
    default Mono<T> findById(K key) {
        return findById(Mono.just(key));
    }

    /**
     * 根据多个ID查询
     *
     * @param key key
     * @return 异步查询结果
     */
    default Flux<T> findById(Collection<K> key) {
        return findById(Flux.fromIterable(key));
    }

    /**
     * 根据异步ID删除数据
     *
     * @param key ID流
     * @return 被删除数量
     */
    Mono<Integer> deleteById(Publisher<K> key);

    /**
     * 根据ID删除数据
     *
     * @param key ID
     * @return 被删除数量
     */
    default Mono<Integer> deleteById(K key) {
        return deleteById(Mono.just(key));
    }

    /**
     * 根据多个ID删除数据
     *
     * @param key ID集合
     * @return 被删除数量
     */
    default Mono<Integer> deleteById(Collection<K> key) {
        return deleteById(Flux.fromIterable(key));
    }

    /**
     * 使用数据流保存数据,如果数据不存在则新增,存在则修改
     *
     * @param data 数据流
     * @return 异步保存结果
     */
    Mono<SaveResult> save(Publisher<T> data);

    /**
     * 保存单个数据,如果数据不存在则新增,存在则修改
     *
     * @param data 数据
     * @return 异步保存结果
     */
    default Mono<SaveResult> save(T data) {
        return save(Mono.just(data));
    }

    /**
     * 保存多个数据,如果数据不存在则新增,存在则修改
     *
     * @param data 数据
     * @return 异步保存结果
     */
    default Mono<SaveResult> save(Collection<T> data) {
        return save(Flux.fromIterable(data));
    }

    /**
     * 根据ID修改数据
     *
     * @param id   ID
     * @param data 数据
     * @return 异步保存结果
     */
    default Mono<Integer> updateById(K id, T data) {
        return updateById(id, Mono.just(data));
    }

    /**
     * 根据ID修改数据
     *
     * @param id   ID
     * @param data 异步数据
     * @return 异步保存结果
     */
    Mono<Integer> updateById(K id, Mono<T> data);

    /**
     * 根据数据流新增数据
     *
     * @param data 数据流
     * @return 异步保存结果
     * @see org.hswebframework.ezorm.rdb.exception.DuplicateKeyException
     */
    Mono<Integer> insert(Publisher<T> data);

    /**
     * 新增数据
     *
     * @param data 数据
     * @return 异步新增结果
     */
    default Mono<Integer> insert(T data) {
        return insert(Mono.just(data));
    }

    /**
     * 新增多个数据
     *
     * @param data 多个数据
     * @return 异步新增结果
     */
    default Mono<Integer> insert(Collection<T> data) {
        return insert(Flux.fromIterable(data));
    }

    /**
     * 批量新增
     *
     * @param data 多个数据
     * @return 异步新增结果
     */
    default Mono<Integer> insertBatch(Collection<T> data) {
        return insertBatch(Mono.just(data));
    }

    /**
     * 执行多个批量新增
     *
     * @param data 多个批量数据流
     * @return 异步新增结果
     */
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
