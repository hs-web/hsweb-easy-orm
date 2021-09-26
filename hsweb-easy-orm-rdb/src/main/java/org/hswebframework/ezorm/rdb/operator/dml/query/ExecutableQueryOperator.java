package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.QuerySqlBuilder;
import reactor.core.publisher.Mono;

public class ExecutableQueryOperator extends BuildParameterQueryOperator {

    private final TableOrViewMetadata metadata;

    public ExecutableQueryOperator(TableOrViewMetadata metadata) {
        super(metadata.getName());
        this.metadata = metadata;
    }

    @Override
    public SqlRequest getSql() {
        return metadata.findFeatureNow(QuerySqlBuilder.ID).build(this.getParameter());
    }

    public Mono<SqlRequest> getSqlAsync() {
        return metadata.findFeatureNow(QuerySqlBuilder.ID).buildAsync(this.getParameter());
    }

    @Override
    public <E, R> QueryResultOperator<E, R> fetch(ResultWrapper<E, R> wrapper) {

        return new DefaultQueryResultOperator<>(this::getSql,
                                                getSqlAsync(),
                                                metadata,
                                                ValueConverterResultWrapper.of(wrapper, metadata));
    }
}
