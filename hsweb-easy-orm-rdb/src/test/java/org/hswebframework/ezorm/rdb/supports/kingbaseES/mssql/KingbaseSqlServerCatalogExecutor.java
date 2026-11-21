package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.executor.DefaultColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

/** Fixed catalog rows passed through the real result wrappers, without a database. */
final class KingbaseSqlServerCatalogExecutor implements SyncSqlExecutor {

    final List<SqlRequest> requests = new ArrayList<>();
    private final Function<SqlRequest, List<Map<String, Object>>> rows;

    KingbaseSqlServerCatalogExecutor(Function<SqlRequest, List<Map<String, Object>>> rows) {
        this.rows = rows;
    }

    @Override
    public int update(SqlRequest request) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void execute(SqlRequest request) {
        throw new UnsupportedOperationException();
    }

    @Override
    public <T, R> R select(SqlRequest request, ResultWrapper<T, R> wrapper) {
        wrap(request, wrapper);
        return wrapper.getResult();
    }

    ReactiveSqlExecutor reactive() {
        return new ReactiveSqlExecutor() {
            @Override
            public Mono<Integer> update(Publisher<SqlRequest> request) {
                return Mono.error(new UnsupportedOperationException());
            }

            @Override
            public Mono<Void> execute(Publisher<SqlRequest> request) {
                return Mono.error(new UnsupportedOperationException());
            }

            @Override
            public <T> Flux<T> select(Publisher<SqlRequest> request, ResultWrapper<T, ?> wrapper) {
                return Flux.from(request).concatMap(sql -> Flux.fromIterable(wrap(sql, wrapper)));
            }
        };
    }

    private <T> List<T> wrap(SqlRequest request, ResultWrapper<T, ?> wrapper) {
        requests.add(request);
        List<Map<String, Object>> data = rows.apply(request);
        wrapper.beforeWrap(() -> data.isEmpty() ? Collections.emptyList() : new ArrayList<>(data.get(0).keySet()));
        List<T> result = new ArrayList<>();
        for (Map<String, Object> values : data) {
            T row = wrapper.newRowInstance();
            int position = 0;
            for (Map.Entry<String, Object> value : values.entrySet()) {
                DefaultColumnWrapperContext<T> context = new DefaultColumnWrapperContext<>(
                    position++, value.getKey(), value.getValue(), row);
                wrapper.wrapColumn(context);
                row = context.getRowInstance();
            }
            boolean next = wrapper.completedWrapRow(row);
            if (!next) {
                break;
            }
            result.add(row);
        }
        wrapper.completedWrap();
        return result;
    }
}
