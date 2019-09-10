package org.hswebframework.ezorm.rdb.operator.dml.query;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.QuerySqlBuilder;

@AllArgsConstructor
public class ExecutableQueryOperator extends BuildParameterQueryOperator {

    private RDBDatabaseMetadata metadata;

    @Override
    public SqlRequest getSql() {
        return metadata.getTableOrView(this.getParameter().getFrom())
                .flatMap(tableOrView -> tableOrView.<QuerySqlBuilder>findFeature(QuerySqlBuilder.id))
                .map(builder -> builder.build(this.getParameter()))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported query operator"));
    }

    @Override
    public <E, R> QueryResultOperator<E, R> fetch(ResultWrapper<E, R> wrapper) {
        return metadata
                .getTableOrView(this.getParameter().getFrom())
                .map(metadata -> new QueryResultOperator<>(getSql(), metadata, wrapper))
                .orElseThrow(() -> new UnsupportedOperationException("table or view [" + getParameter().getFrom() + "] doesn't exist "));
    }
}
