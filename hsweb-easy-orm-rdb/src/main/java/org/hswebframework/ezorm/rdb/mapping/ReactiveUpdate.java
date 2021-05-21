package org.hswebframework.ezorm.rdb.mapping;

import reactor.core.publisher.Mono;

import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * 响应式DSL更新接口
 *
 * @param <E> 实体类型
 * @author zhouhao
 * @version 1.0
 * @see org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator
 */
public interface ReactiveUpdate<E> extends DSLUpdate<E, ReactiveUpdate<E>> {

    /**
     * 执行更新
     *
     * @return 更新结果
     */
    Mono<Integer> execute();

    /**
     * 在执行更新后做一些响应式操作,在操作时可以获取到Update,比如更新缓存.
     *
     * <pre>
     *     createUpdate()
     *      .set("state",1)
     *      .where()
     *      .in("id",idList)
     *      .onExecute((update,result)->{
     *        return result
     *                .flatMap(i-> this
     *                  .createQuery()
     *                  .setParam(update.toQueryParam())//获取到update的where参数
     *                  .fetch()
     *                  .flatMap(this::clearCache)
     *                  .thenReturn(i);
     *      }).execute();
     *
     * </pre>
     *
     * @param consumer 执行结果处理器
     * @return this
     */
    ReactiveUpdate<E> onExecute(BiFunction<ReactiveUpdate<E>, Mono<Integer>, Mono<Integer>> consumer);

}
