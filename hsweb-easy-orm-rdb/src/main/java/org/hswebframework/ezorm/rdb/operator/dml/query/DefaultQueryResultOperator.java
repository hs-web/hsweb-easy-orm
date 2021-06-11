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

import java.util.function.Supplier;

class DefaultQueryResultOperator<E, R> implements QueryResultOperator<E, R> {

    private final Supplier<SqlRequest> sqlRequest;
    private final Mono<SqlRequest> asyncSql;
    private final RDBDatabaseMetadata metadata;
    private final ResultWrapper<E, R> wrapper;

    public DefaultQueryResultOperator(Supplier<SqlRequest> sqlRequest,
                                      Mono<SqlRequest> asyncSql,
                                      TableOrViewMetadata tableOrViewMetadata,
                                      ResultWrapper<E, R> wrapper) {
        this.sqlRequest = sqlRequest;
        this.asyncSql = asyncSql;
        this.metadata = tableOrViewMetadata.getSchema().getDatabase();
        this.wrapper = wrapper;
    }

    protected ResultWrapper<E, R> getWrapper() {

        return wrapper;
    }

    @Override
    public R sync() {
        return ExceptionUtils
                .translation(() -> metadata
                        .findFeatureNow(SyncSqlExecutor.ID)
                        .select(sqlRequest.get(), getWrapper()), metadata);
    }

    @Override
    @SuppressWarnings("all")
    public Flux<E> reactive() {
        return Flux
                .defer(() -> {
                    return metadata
                            .findFeatureNow(ReactiveSqlExecutor.ID)
                            .select(asyncSql, getWrapper())
                            .onErrorMap(error -> ExceptionUtils.translation(metadata, error));
                });
    }
}
