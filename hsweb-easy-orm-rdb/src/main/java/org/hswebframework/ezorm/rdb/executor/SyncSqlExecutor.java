package org.hswebframework.ezorm.rdb.executor;

import org.hswebframework.ezorm.core.ObjectWrapper;

import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

public interface SyncSqlExecutor {

    int update(SqlRequest request);

    boolean execute(SqlRequest request);

    <T> List<T> select(SqlRequest request, ObjectWrapper<T> wrapper);

    <T> int select(SqlRequest request, ObjectWrapper<T> wrapper, Function<T,Boolean> consumer);

    <T> T selectSingle(SqlRequest request, ObjectWrapper<T> wrapper);

}
