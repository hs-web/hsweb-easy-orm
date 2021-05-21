package org.hswebframework.ezorm.rdb.mapping;

import reactor.core.publisher.Mono;

import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * 响应式动态删除接口
 *
 * @author zhouhao
 * @since 4.0.0
 */
public interface ReactiveDelete extends DSLDelete<ReactiveDelete> {
    /**
     * 执行异步删除,返回被删除的数据条数
     *
     * @return 异步删除结果
     */
    Mono<Integer> execute();

    /**
     * 在执行更新后做一些响应式操作,在操作时可以获取到Update,比如更新缓存.
     *
     * <pre>
     *     createDelete()
     *      .where()
     *      .in("id",idList)
     *      .onExecute((delete,execute)->{
     *        return createQuery()
     *                 .setParam(delete.toQueryParam())//获取到update的where参数
     *                 .fetch()
     *                 .flatMap(this::clearCache)
     *                 .then(execute)
     *             ;
     *      }).execute();
     *
     * </pre>
     *
     * @param mapper 执行结果处理器
     * @return this
     */
    ReactiveDelete onExecute(BiFunction<
            /*当前动态删除接口*/ ReactiveDelete,
            /*   删除执行器  */ Mono<Integer>,
            /*  新的执行器   */ Mono<Integer>> mapper);
}
