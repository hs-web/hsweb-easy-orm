package org.hswebframework.ezorm.rdb.executor;

import org.hswebframework.ezorm.core.ObjectWrapper;

import java.util.List;
import java.util.concurrent.CompletionStage;
import java.util.function.Consumer;

public interface AsyncSqlExecutor {

    CompletionStage<Integer> update(SqlRequest request);

    CompletionStage<Void> execute(SqlRequest request);

    <T> CompletionStage<List<T>> select(SqlRequest request, ObjectWrapper<T> wrapper);

    <T> CompletionStage<Integer> select(SqlRequest request, ObjectWrapper<T> wrapper, Consumer<T> consumer);

    <T> CompletionStage<T> selectOne(SqlRequest request, ObjectWrapper<T> wrapper);


}
