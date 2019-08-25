package org.hswebframework.ezorm.rdb.executor;

import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;

import java.util.concurrent.CompletionStage;

public interface AsyncSqlExecutor {

    CompletionStage<Integer> update(SqlRequest request);

    CompletionStage<Void> execute(SqlRequest request);

    <T,R> CompletionStage<R> select(SqlRequest request, ResultWrapper<T,R> wrapper);

}
