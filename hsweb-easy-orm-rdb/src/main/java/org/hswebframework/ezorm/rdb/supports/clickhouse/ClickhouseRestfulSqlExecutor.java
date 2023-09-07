package org.hswebframework.ezorm.rdb.supports.clickhouse;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.DefaultColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @className ClickhouseRestfulSqlExecutor
 * @Description TODO
 * @Author dengpengyu
 * @Date 2023/9/4 14:40
 * @Vesion 1.0
 */
public class ClickhouseRestfulSqlExecutor implements ReactiveSqlExecutor {
    private Logger log = LoggerFactory.getLogger(ClickhouseRestfulSqlExecutor.class);
    private WebClient client;

    public ClickhouseRestfulSqlExecutor(WebClient client) {
        this.client = client;
    }

    @Override
    public Mono<Integer> update(Publisher<SqlRequest> request) {
        return this
                .doExecute(request)
                .then(Mono.just(1));
    }

    @Override
    public Mono<Void> execute(Publisher<SqlRequest> request) {
        return this
                .doExecute(request)
                .then();
    }

    @Override
    public <E> Flux<E> select(Publisher<SqlRequest> request, ResultWrapper<E, ?> wrapper) {

        return this
                .doExecute(request)
                .flatMap(response -> convertQueryResult(response, wrapper));
    }

    private Flux<JSONObject> doExecute(Publisher<SqlRequest> requests) {
        return Flux
                .from(requests)
                .expand(request -> {
                    if (request instanceof BatchSqlRequest) {
                        return Flux.fromIterable(((BatchSqlRequest) request).getBatch());
                    }
                    return Flux.empty();
                })

                .filter(SqlRequest::isNotEmpty)
                .concatMap(request -> {
                    String sql;
                    if (request.toNativeSql().toUpperCase().startsWith("INSERT")
                            || request.toNativeSql().toUpperCase().startsWith("ALTER")) {
                        sql = request.toNativeSql();
                    } else {
                        sql = request.toNativeSql() + " FORMAT JSON";
                    }
                    log.info("Execute ==> {}", sql);
                    return client
                            .post()
                            .bodyValue(sql)
                            .exchangeToMono(response -> response
                                    .bodyToMono(String.class)
                                    .map(json -> {
                                        checkExecuteResult(sql, json);
                                        JSONObject result = JSON.parseObject(json);

                                        return result;
                                    }));
                });
    }

    private void checkExecuteResult(String sql, String code) {
        if (code.startsWith("Code")) {
            throw new RuntimeException(code);
        }
    }

    protected <E> Flux<E> convertQueryResult(JSONObject result, ResultWrapper<E, ?> wrapper) {

        JSONArray head = result.getJSONArray("meta");
        JSONArray data = result.getJSONArray("data");

        if (CollectionUtils.isEmpty(head) || CollectionUtils.isEmpty(data)) {
            return Flux.empty();
        }
        List<String> columns = head.stream()
                .map(v -> ((JSONObject) v).get("name").toString())
                .collect(Collectors.toList());

        return Flux.create(sink -> {
            wrapper.beforeWrap(() -> columns);

            for (Object rowo : data) {
                E rowInstance = wrapper.newRowInstance();
                JSONObject row = (JSONObject) rowo;
                for (int i = 0; i < columns.size(); i++) {
                    String property = columns.get(i);
                    Object value = row.get(property);
                    DefaultColumnWrapperContext<E> context = new DefaultColumnWrapperContext<>(i, property, value, rowInstance);
                    wrapper.wrapColumn(context);
                    rowInstance = context.getRowInstance();
                }
                if (!wrapper.completedWrapRow(rowInstance)) {
                    break;
                }
                if (rowInstance != null) {
                    sink.next(rowInstance);
                }
            }
            wrapper.completedWrap();
            sink.complete();
        });
    }
}
