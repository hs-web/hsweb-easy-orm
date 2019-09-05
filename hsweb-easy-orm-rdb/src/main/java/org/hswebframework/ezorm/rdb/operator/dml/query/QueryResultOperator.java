package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.rdb.executor.AsyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.concurrent.CompletionStage;

public class QueryResultOperator<E, R> implements ResultOperator<E, R> {

    private SqlRequest sqlRequest;
    private RDBDatabaseMetadata metadata;
    private ResultWrapper<E, R> wrapper;

    public QueryResultOperator(SqlRequest sqlRequest,
                               TableOrViewMetadata tableOrViewMetadata,
                               ResultWrapper<E, R> wrapper) {
        this.sqlRequest = sqlRequest;
        this.metadata = tableOrViewMetadata.getSchema().getDatabase();

        this.wrapper = wrapper;
    }

    protected ResultWrapper<E, R> getWrapper() {

        return wrapper;
    }

    @Override
    public R sync() {
        return metadata.<SyncSqlExecutor>getFeature(SyncSqlExecutor.id)
                .map(executor -> executor.select(sqlRequest, getWrapper()))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported SyncSqlExecutor"));
    }

    @Override
    public CompletionStage<R> async() {
        return metadata.<AsyncSqlExecutor>getFeature(AsyncSqlExecutor.id)
                .map(executor -> executor.select(sqlRequest, getWrapper()))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported AsyncSqlExecutor"));
    }

    @Override
    @SuppressWarnings("all")
    public Flux<E> reactive() {
        return metadata.<ReactiveSqlExecutor>getFeature(ReactiveSqlExecutor.id)
                .map(executor -> executor.select(Mono.just(sqlRequest),  getWrapper()))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported ReactiveSqlExecutor"));
    }
}
