package org.hswebframework.ezorm.rdb.executor;

import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;

public interface SyncSqlExecutor extends Feature {

    String id = "syncSqlExecutor";

    @Override
    default String getId() {
        return id;
    }

    @Override
    default String getName() {
        return "同步SQL执行器";
    }

    @Override
    default RDBFeatureType getType() {
        return RDBFeatureType.sqlExecutor;
    }

    int update(SqlRequest request);

    void execute(SqlRequest request);

    <T,R> R select(SqlRequest request, ResultWrapper<T,R> wrapper);

}
