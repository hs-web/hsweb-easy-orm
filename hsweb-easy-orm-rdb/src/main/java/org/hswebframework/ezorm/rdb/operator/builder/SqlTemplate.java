package org.hswebframework.ezorm.rdb.operator.builder;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;

public interface SqlTemplate {

    String getTemplate();

    SqlRequest render(Object parameter);
}
