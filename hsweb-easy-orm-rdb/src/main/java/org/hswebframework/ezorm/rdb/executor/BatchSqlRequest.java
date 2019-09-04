package org.hswebframework.ezorm.rdb.executor;

import java.util.List;

public interface BatchSqlRequest extends SqlRequest {


    List<SqlRequest> getBatch();

}
