package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.QuerySqlBuilder;

public class ExecutableQueryOperator extends BuildParameterQueryOperator {

    private TableOrViewMetadata metadata;

    public ExecutableQueryOperator(TableOrViewMetadata metadata) {
        super(metadata.getName());
        this.metadata = metadata;
    }

    @Override
    public SqlRequest getSql() {
        return metadata.findFeature(QuerySqlBuilder.ID)
                .map(builder -> builder.build(this.getParameter()))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported query operator"));
    }

    @Override
    public <E, R> QueryResultOperator<E, R> fetch(ResultWrapper<E, R> wrapper) {
        return new QueryResultOperator<>(getSql(), metadata, wrapper);
    }
}
