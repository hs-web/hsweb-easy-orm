package org.hswebframework.ezorm.rdb.executor;

import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;

public interface SyncSqlExecutor {

    int update(SqlRequest request);

    void execute(SqlRequest request);

    <T,R> R select(SqlRequest request, ResultWrapper<T,R> wrapper);

}
