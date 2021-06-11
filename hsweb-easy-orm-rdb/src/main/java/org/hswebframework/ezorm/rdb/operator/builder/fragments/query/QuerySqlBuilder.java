package org.hswebframework.ezorm.rdb.operator.builder.fragments.query;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import reactor.core.publisher.Mono;

public interface QuerySqlBuilder extends SqlBuilder<QueryOperatorParameter> {

    String ID_VALUE = "querySqlBuilder";

    FeatureId<QuerySqlBuilder> ID = FeatureId.of(ID_VALUE);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return "查询SQL构造器";
    }

    SqlRequest build(QueryOperatorParameter parameter);

    Mono<SqlRequest> buildAsync(QueryOperatorParameter parameter);
}
