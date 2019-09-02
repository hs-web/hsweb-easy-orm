package org.hswebframework.ezorm.rdb.operator.builder.fragments.query;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;

public interface QuerySqlBuilder extends SqlBuilder<QueryOperatorParameter> {

    String id = "querySqlBuilder";

    @Override
    default String getId() {
        return id;
    }

    @Override
    default String getName() {
        return "查询SQL构造器";
    }

    SqlRequest build(QueryOperatorParameter parameter);


}
