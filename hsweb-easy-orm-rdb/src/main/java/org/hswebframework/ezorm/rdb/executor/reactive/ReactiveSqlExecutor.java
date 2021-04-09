package org.hswebframework.ezorm.rdb.executor.reactive;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Map;

public interface ReactiveSqlExecutor extends Feature {

    String ID_VALUE = "reactiveSqlExecutor";
    FeatureId<ReactiveSqlExecutor> ID = FeatureId.of(ID_VALUE);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return "响应式SQL执行器";
    }

    @Override
    default RDBFeatureType getType() {
        return RDBFeatureType.sqlExecutor;
    }

    /**
     * 执行更新语句,支持 update,delete,insert
     *
     * @param request SQL请求
     * @return 更新数量
     */
    Mono<Integer> update(Publisher<SqlRequest> request);

    /**
     * 执行SQL语句,忽略结果.
     *
     * @param request SQL请求
     * @return void
     */
    Mono<Void> execute(Publisher<SqlRequest> request);

    /**
     * 执行查询语句,并使用同一个包装器包装返回结果
     *
     * @param request 查询请求流
     * @param wrapper 结果包装器
     * @param <E>结果类型
     * @return 结果流
     */
    <E> Flux<E> select(Publisher<SqlRequest> request, ResultWrapper<E, ?> wrapper);

    /**
     * 执行SQL,忽略结果,通常用于执行DDL语句.
     *
     * @param request SqlRequest
     * @return 更新结果数量
     * @see SqlRequests
     */
    default Mono<Void> execute(SqlRequest request) {
        return execute(Mono.just(request));
    }

    /**
     * 使用SQL请求执行更新
     *
     * @param request SqlRequest
     * @return 更新结果数量
     * @see SqlRequests
     */
    default Mono<Integer> update(SqlRequest request) {
        return update(Mono.just(request));
    }

    /**
     * 执行更新语句
     * <pre>
     *     update("update my_table set name =? where id = ?",name,id);
     * </pre>
     *
     * @param sql  SQL语句
     * @param args 预编译参数
     * @return 更新结果数量
     */
    default Mono<Integer> update(String sql, Object... args) {
        return update(SqlRequests.of(sql, args));
    }

    /**
     * 直接执行查询SQL语句,不支持预编译参数,如果是带查询条件的SQL,请使用{@link ReactiveSqlExecutor#select(String, Object...)}
     *
     * @param sql     SQL语句
     * @param wrapper 结果包装器
     * @param <E>     结果类型
     * @return 结果流
     */
    default <E> Flux<E> select(String sql, ResultWrapper<E, ?> wrapper) {
        return select(SqlRequests.of(sql), wrapper);
    }

    /**
     * 使用预编译执行查询SQL,并返回map结果.
     * <pre>
     *     select("select * from my_table where name = ?",name);
     * </pre>
     *
     * @param sql  查询SQL
     * @param args 预编译参数
     * @return 结果流
     * @see ResultWrappers#map()
     */
    default Flux<Map<String, Object>> select(String sql, Object... args) {
        return select(SqlRequests.of(sql, args), ResultWrappers.map());
    }

    /**
     * 使用SQL请求对象来进行查询,并使用指定的包装器来包装返回结果
     * <pre>
     *     select(SqlRequests.of("select * from my_table where name = ?"),ResultWrappers.map());
     * </pre>
     *
     * @param sqlRequest 查询SQL
     * @param wrapper    结果包装器
     * @return 结果流
     * @see ResultWrappers#map()
     * @see SqlRequests#of(String, Object...)
     * @see SqlRequests#template(String, Object)
     */
    default <E> Flux<E> select(SqlRequest sqlRequest, ResultWrapper<E, ?> wrapper) {
        return select(Mono.just(sqlRequest), wrapper);
    }

}
