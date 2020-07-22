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

    Mono<Integer> update(Publisher<SqlRequest> request);

    Mono<Void> execute(Publisher<SqlRequest> request);

    <E> Flux<E> select(Publisher<SqlRequest> request, ResultWrapper<E, ?> wrapper);

    default Mono<Integer> update(SqlRequest request) {
        return update(Mono.just(request));
    }

    default Mono<Integer> update(String sql, Object... args) {
        return update(SqlRequests.of(sql, args));
    }

    default <E> Flux<E> select(String sql, ResultWrapper<E, ?> wrapper) {
        return select(SqlRequests.of(sql), wrapper);
    }

    default Flux<Map<String, Object>> select(String sql, Object... args) {
        return select(SqlRequests.of(sql, args), ResultWrappers.map());
    }

    default <E> Flux<E> select(SqlRequest sqlRequest, ResultWrapper<E, ?> wrapper) {
        return select(Mono.just(sqlRequest), wrapper);
    }

}
