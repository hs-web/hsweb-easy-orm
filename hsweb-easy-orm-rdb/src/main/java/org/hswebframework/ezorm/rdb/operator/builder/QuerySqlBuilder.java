package org.hswebframework.ezorm.rdb.operator.builder;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;

public interface QuerySqlBuilder extends SqlBuilder<ComplexQueryParameter> {

    String id = "querySqlBuilder";

    @Override
    default String getId() {
        return id;
    }

    @Override
    default String getName() {
        return "查询SQL构造器";
    }

    SqlRequest build(ComplexQueryParameter parameter);


}
