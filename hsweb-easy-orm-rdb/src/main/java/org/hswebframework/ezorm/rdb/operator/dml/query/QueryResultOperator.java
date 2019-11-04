package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import org.hswebframework.ezorm.rdb.utils.ExceptionUtils;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

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
        return ExceptionUtils.translation(() -> metadata
                .getFeature(SyncSqlExecutor.ID)
                .orElseThrow(() -> new UnsupportedOperationException("unsupported SyncSqlExecutor"))
                .select(sqlRequest, getWrapper()), metadata);
    }

    @Override
    @SuppressWarnings("all")
    public Flux<E> reactive() {
        return Flux.defer(() -> {
            return metadata.getFeature(ReactiveSqlExecutor.ID)
                    .orElseThrow(() -> new UnsupportedOperationException("unsupported ReactiveSqlExecutor"))
                    .select(Mono.just(sqlRequest), getWrapper())
                    .onErrorMap(error -> ExceptionUtils.translation(metadata, error));
        });
    }
}
