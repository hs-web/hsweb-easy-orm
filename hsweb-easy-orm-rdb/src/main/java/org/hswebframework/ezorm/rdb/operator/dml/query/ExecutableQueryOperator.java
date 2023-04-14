package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ConvertResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.SingleColumnResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.SingleResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.QuerySqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import reactor.core.publisher.Mono;

public class ExecutableQueryOperator extends BuildParameterQueryOperator {

    private final TableOrViewMetadata metadata;

    private ExecutableQueryOperator(QueryOperatorParameter parameter,
                                    TableOrViewMetadata metadata) {
        super(parameter);
        this.metadata = metadata;
        parameter.setFrom(metadata.getName());

    }

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
                                                wrapWrapper(wrapper));
    }

    private <E, R> ResultWrapper<E, R> wrapWrapper(ResultWrapper<E, R> wrapper) {
        if (wrapper instanceof SingleColumnResultWrapper) {
            return wrapper;
        }
        return ValueConverterResultWrapper.of(wrapper, metadata);
    }

    @Override
    @SuppressWarnings("all")
    public QueryOperator clone() {
        return new ExecutableQueryOperator(parameter.clone(), metadata);
    }
}
