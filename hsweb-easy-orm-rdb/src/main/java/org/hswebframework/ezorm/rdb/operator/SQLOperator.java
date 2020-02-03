package org.hswebframework.ezorm.rdb.operator;

import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;

public interface SQLOperator {

    SyncSqlExecutor sync();

    ReactiveSqlExecutor reactive();

}
