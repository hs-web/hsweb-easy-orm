package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.ReactiveQuery;
import org.hswebframework.ezorm.rdb.mapping.events.DefaultReactiveResultHolder;
import org.hswebframework.ezorm.rdb.mapping.events.EventSupportWrapper;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.mapping.events.ReactiveResultHolder;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.DMLOperator;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.function.BiFunction;
import java.util.function.Function;

import static org.hswebframework.ezorm.rdb.events.ContextKeys.source;
import static org.hswebframework.ezorm.rdb.events.ContextKeys.tableMetadata;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;
import static org.hswebframework.ezorm.rdb.mapping.events.EventSupportWrapper.*;
import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.*;
import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.type;
import static org.hswebframework.ezorm.rdb.operator.dml.query.Selects.count1;

public class DefaultReactiveQuery<T> extends DefaultQuery<T, ReactiveQuery<T>> implements ReactiveQuery<T> {

    private final Logger logger;

    public DefaultReactiveQuery(TableOrViewMetadata tableMetadata,
                                EntityColumnMapping mapping,
                                DMLOperator operator,
                                ResultWrapper<T, ?> wrapper,
                                Logger logger,
                                ContextKeyValue<?>... keyValues) {
        super(tableMetadata, mapping, operator, wrapper, keyValues);
        this.logger = logger;
    }

    @Override
    public Flux<T> fetch() {
        return this
                .doFetch(operator.query(tableMetadata),
                         "fetch",
                         (_queryOperator) -> _queryOperator
                                 .context(param.getContext())
                                 .select(getSelectColumn())
                                 .where(param.getTerms())
                                 .orderBy(getSortOrder())
                                 .when(param.isPaging(), query -> query.paging(param.getPageIndex(), param.getPageSize()))
                                 .fetch(eventWrapper(tableMetadata, wrapper, executorType("reactive"), type("fetch")))
                                 .reactive())
                .contextWrite(ctx->ctx.put(Logger.class,logger));
    }

    @Override
    public Mono<T> fetchOne() {
        return this
                .doFetch(operator.query(tableMetadata),
                         "fetchOne",
                         (_queryOperator) -> _queryOperator
                                 .context(param.getContext())
                                 .select(getSelectColumn())
                                 .where(param.getTerms())
                                 .orderBy(getSortOrder())
                                 .paging(0, 1)
                                 .fetch(eventWrapper(tableMetadata, wrapper, executorType("reactive"), type("fetchOne")))
                                 .reactive())
                .contextWrite(ctx-> ctx.put(Logger.class,logger))
                .singleOrEmpty();
    }

    private <O> Flux<O> doFetch(QueryOperator queryOperator, String type, Function<QueryOperator, Publisher<O>> executor) {
        DefaultReactiveResultHolder holder = new DefaultReactiveResultHolder();
        tableMetadata
                .fireEvent(MappingEventTypes.select_before, eventContext ->
                        eventContext.set(
                                source(DefaultReactiveQuery.this),
                                query(queryOperator),
                                dml(operator),
                                tableMetadata(tableMetadata),
                                columnMapping(columnMapping),
                                reactiveResultHolder.value(holder),
                                queryOaram.value(param),
                                executorType("reactive"),
                                type(type)
                        ));
        return holder
                .doBefore()
                .thenMany(Flux.defer(() -> executor.apply(queryOperator)));
    }

    @Override
    public Mono<Integer> count() {
        QueryOperator queryOperator = operator
                .query(tableMetadata)
                .select(count1().as("_total"));
        return this
                .doFetch(queryOperator, "count", _opt -> _opt
                        .context(param.getContext())
                        .where(param.getTerms())
                        .fetch(column("_total", Number.class::cast))
                        .reactive()
                        .map(Number::intValue)
                        .reduce(Math::addExact)
                        .switchIfEmpty(Mono.just(0)))
                .contextWrite(ctx-> ctx.put(Logger.class,logger))
                .singleOrEmpty();
    }

}
