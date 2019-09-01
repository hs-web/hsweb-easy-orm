package org.hswebframework.ezorm.rdb.operator;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.AsyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.meta.DefaultRDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.ExecutableQueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;

@AllArgsConstructor
public class DefaultDatabaseOperator
        implements DatabaseOperator, DMLOperator, SQLOperator {

    private DefaultRDBDatabaseMetadata metadata;

    @Override
    public DefaultRDBDatabaseMetadata getMetadata() {
        return metadata;
    }

    @Override
    public DMLOperator dml() {
        return this;
    }

    @Override
    public DDLOperator ddl() {
        return null;
    }

    @Override
    public SQLOperator sql() {
        return this;
    }

    @Override
    public QueryOperator query() {
        return new ExecutableQueryOperator(metadata);
    }

    @Override
    public SyncSqlExecutor sync() {
        return metadata.<SyncSqlExecutor>getFeature(SyncSqlExecutor.id)
                .orElseThrow(() -> new UnsupportedOperationException("unsupported SyncSqlExecutor"));
    }

    @Override
    public AsyncSqlExecutor async() {
        return metadata.<AsyncSqlExecutor>getFeature(AsyncSqlExecutor.id)
                .orElseThrow(() -> new UnsupportedOperationException("unsupported AsyncSqlExecutor"));
    }

    @Override
    public ReactiveSqlExecutor reactive() {
        return metadata.<ReactiveSqlExecutor>getFeature(ReactiveSqlExecutor.id)
                .orElseThrow(() -> new UnsupportedOperationException("unsupported ReactiveSqlExecutor"));
    }
}
