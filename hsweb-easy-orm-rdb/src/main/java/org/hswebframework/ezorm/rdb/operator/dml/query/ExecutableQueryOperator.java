package org.hswebframework.ezorm.rdb.operator.dml.query;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.meta.DefaultRDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.QuerySqlBuilder;

@AllArgsConstructor
public class ExecutableQueryOperator extends BuildParameterQueryOperator {

    private DefaultRDBDatabaseMetadata metadata;

    @Override
    public SqlRequest getSql() {
        return metadata.getTableOrView(this.getParameter().getFrom())
                .flatMap(tableOrView -> tableOrView.<QuerySqlBuilder>getFeature(QuerySqlBuilder.id))
                .map(builder -> builder.build(this.getParameter()))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported query operator"));
    }

    @Override
    public <E, R> ResultOperator<E, R> fetch(ResultWrapper<E, R> wrapper) {
        String from = this.getParameter().getFrom();
        TableOrViewMetadata tableOrViewMetadata = metadata.getTableOrView(this.getParameter().getFrom())
                .orElseThrow(() -> new UnsupportedOperationException("table or view [" + from + "] doesn't exist "));

        return new QueryResultOperator<>(getSql(), tableOrViewMetadata, wrapper);
    }
}
