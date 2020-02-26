package org.hswebframework.ezorm.rdb.executor.reactive;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface ReactiveSqlExecutor  extends Feature {

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

}
