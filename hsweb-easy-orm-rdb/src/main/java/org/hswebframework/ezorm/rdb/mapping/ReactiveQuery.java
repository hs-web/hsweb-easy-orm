package org.hswebframework.ezorm.rdb.mapping;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/**
 * 响应式动态查询接口
 *
 * @param <T> 实体类类型
 * @author zhouhao
 */
public interface ReactiveQuery<T> extends DSLQuery<ReactiveQuery<T>> {

    /**
     * 执行查询并获取返回数据流,如果未查询到结果将返回{@link Flux#empty()}
     *
     * @return 查询结果流
     */
    Flux<T> fetch();

    /**
     * 执行count查询,并返回count查询结果.
     *
     * @return count结果
     */
    Mono<Integer> count();

    /**
     * 执行查询并返回单个数据
     *
     * @return 如果未查询到结果将返回{@link Mono#empty()}
     */
    Mono<T> fetchOne();

    /**
     * 复制当前查询对象,在需要多次设置查询条件时,可以使用此方法复制一个新的查询对象
     *
     * @return 新的查询对象
     * @since 4.1.3
     */
    default ReactiveQuery<T> copy() {
        return this;
    }
}
