package org.hswebframework.ezorm.rdb.executor;

import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;

import java.util.concurrent.CompletionStage;

public interface AsyncSqlExecutor extends Feature {
    String id = "asyncSqlExecutor";

    @Override
    default String getId() {
        return id;
    }

    @Override
    default String getName() {
        return "异步SQL执行器";
    }

    @Override
    default RDBFeatureType getType() {
        return RDBFeatureType.sqlExecutor;
    }

    CompletionStage<Integer> update(SqlRequest request);

    CompletionStage<Void> execute(SqlRequest request);

    <T, R> CompletionStage<R> select(SqlRequest request, ResultWrapper<T, R> wrapper);

}
