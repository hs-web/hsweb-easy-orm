package org.hswebframework.ezorm.rdb.executor.reactive.r2dbc;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.Result;
import io.r2dbc.spi.Statement;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.CastUtil;
import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.DefaultColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import reactor.core.publisher.*;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.hswebframework.ezorm.rdb.utils.SqlUtils.*;

/**
 * 基于r2dbc的响应式sql执行器
 *
 * @author zhouhao
 * @since 4.0
 */
@Slf4j
public abstract class R2dbcReactiveSqlExecutor implements ReactiveSqlExecutor {

    @Getter
    @Setter
    private Logger logger = R2dbcReactiveSqlExecutor.log;

    /**
     * 获取连接
     *
     * @return r2dbc Connection
     */
    protected abstract Mono<Connection> getConnection();

    /**
     * 释放连接，不建议实现此方法，推荐在getConnection里使用{@link Mono#usingWhen(Publisher, Function, Function)}来处理.
     *
     * @param type       type
     * @param connection connection
     */
    protected abstract void releaseConnection(SignalType type, Connection connection);

    enum Interrupted {
        instance;

        static boolean nonInterrupted(Object o) {
            return o != instance;
        }
    }

    /**
     * 使用指定的Connection执行SQL并返回执行结果
     *
     * @param connection Connection
     * @param request    SQL
     * @return 执行结果
     */
    protected Flux<Result> doExecute(Connection connection,
                                     SqlRequest request) {

        return Flux
                .from(this.prepareStatement(connection.createStatement(request.getSql()), request)
                          .execute())
                .map(Result.class::cast)
                .doOnSubscribe(subscription -> printSql(logger, request))
                .doOnError(err -> logger.error("==>      Error: {}", request.toNativeSql(), err));
    }

    /**
     * 执行SQL并返回执行结果,多个SQL将使用同一个Connection执行
     *
     * @param sqlRequestFlux SQL流
     * @return 执行结果
     */
    private Flux<Result> doExecute(Flux<SqlRequest> sqlRequestFlux) {
        return this
                .getConnection()
                .flatMapMany(connection -> sqlRequestFlux
                        .concatMap(sqlRequest -> this.doExecute(connection, sqlRequest))
                        .doFinally(type -> releaseConnection(type, connection)));
    }

    @Override
    public Mono<Integer> update(Publisher<SqlRequest> request) {
        return this
                .doExecute(toFlux(request))
                .flatMap(result -> Mono.from(result.getRowsUpdated()).defaultIfEmpty(0))
                .doOnNext(count -> logger.debug("==>    Updated: {}", count))
                .collect(Collectors.summingInt(Integer::intValue))
                .defaultIfEmpty(0);
    }

    @Override
    public Mono<Void> execute(Publisher<SqlRequest> request) {
        return this.doExecute(toFlux(request))
                   .flatMap(Result::getRowsUpdated)
                   .then();
    }

    @Override
    public <E> Flux<E> select(Publisher<SqlRequest> request, ResultWrapper<E, ?> wrapper) {
        return this
                .toFlux(request)
                .as(this::doExecute)
                .flatMap(result -> result
                        .map((row, meta) -> {
                            //查询结果的列名
                            List<String> columns = new ArrayList<>(meta.getColumnNames());
                            wrapper.beforeWrap(() -> columns);
                            E e = wrapper.newRowInstance();
                            for (int i = 0, len = columns.size(); i < len; i++) {
                                String column = columns.get(i);

                                DefaultColumnWrapperContext<E> context = new DefaultColumnWrapperContext<>(i, column, row.get(column), e);

                                wrapper.wrapColumn(context);
                                e = context.getRowInstance();
                            }
                            //中断转换
                            if (!wrapper.completedWrapRow(e)) {
                                return Interrupted.instance;
                            }
                            return e;
                        }))
                .takeWhile(Interrupted::nonInterrupted)
                .map(CastUtil::<E>cast)
                .doOnCancel(wrapper::completedWrap)
                .doOnComplete(wrapper::completedWrap);
    }

    /**
     * 将SQL流转为Flux
     *
     * @param request
     * @return
     */
    @SuppressWarnings("all")
    protected Flux<SqlRequest> toFlux(Publisher<SqlRequest> request) {

        return Flux
                .from(request)
                .flatMap(sql -> {
                    //批量SQL
                    if (sql instanceof BatchSqlRequest) {
                        return Flux.concat(Flux.just(sql), Flux.fromIterable(((BatchSqlRequest) sql).getBatch()));
                    }
                    return Flux.just(sql);
                })
                //忽略空SQL
                .filter(SqlRequest::isNotEmpty)
                .map(this::convertRequest);
    }

    /**
     * 转换SQL为R2dbcSqlRequest,由于不同数据库的预编译占位符不同,需要进行转换
     *
     * @param sqlRequest SqlRequest
     * @return SqlRequest
     */
    protected SqlRequest convertRequest(SqlRequest sqlRequest) {
        return R2dbcSqlRequest.of(getBindFirstIndex(), getBindSymbol(), sqlRequest);
    }

    /**
     * @return 预编译参数绑定占位符
     */
    protected String getBindSymbol() {
        return "$";
    }

    /**
     * @return 预编译参数绑定起始索引
     */
    protected int getBindFirstIndex() {
        return 1;
    }

    protected void bindNull(Statement statement, int index, Class<?> type) {
        if (type == Date.class) {
            type = LocalDateTime.class;
        }
        statement.bindNull(getBindSymbol() + (index + getBindFirstIndex()), type);
    }

    protected void bind(Statement statement, int index, Object value) {
        //时间需要转换为LocalDateTime
        if (value instanceof Date) {
            value = ((Date) value)
                    .toInstant()
                    .atZone(ZoneOffset.systemDefault())
                    .toLocalDateTime();
        }
        statement.bind(getBindSymbol() + (index + getBindFirstIndex()), value);
    }

    protected Statement prepareStatement(Statement statement, SqlRequest request) {
        if (request.isEmpty() || request.getParameters() == null) {
            return statement;
        }
        int index = 0;
        for (Object parameter : request.getParameters()) {
            if (parameter == null) {
                bindNull(statement, index, String.class);
            } else if (parameter instanceof NullValue) {
                bindNull(statement, index, ((NullValue) parameter).getDataType().getJavaType());
            } else {
                bind(statement, index, parameter);
            }
            index++;
        }
        return statement;
    }
}
