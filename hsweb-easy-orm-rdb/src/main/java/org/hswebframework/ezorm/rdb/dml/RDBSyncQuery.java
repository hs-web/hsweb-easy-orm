package org.hswebframework.ezorm.rdb.dml;

import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.UpdateParam;

import java.util.List;

public interface RDBSyncQuery<T> {


    T selectSingle(QueryParam param);

    List<T> select(QueryParam param);

    int count(QueryParam param);

    T insert(T data);

    T update(UpdateParam param);

}
