package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.ReactiveQuery;
import org.hswebframework.ezorm.rdb.mapping.events.EventSupportWrapper;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.DMLOperator;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import static org.hswebframework.ezorm.rdb.events.ContextKeys.source;
import static org.hswebframework.ezorm.rdb.events.ContextKeys.tableMetadata;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;
import static org.hswebframework.ezorm.rdb.mapping.events.EventSupportWrapper.*;
import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.*;
import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.type;
import static org.hswebframework.ezorm.rdb.operator.dml.query.Selects.count1;

public class DefaultReactiveQuery<T> extends DefaultQuery<T, ReactiveQuery<T>> implements ReactiveQuery<T> {

    public DefaultReactiveQuery(TableOrViewMetadata tableMetadata,
                                EntityColumnMapping mapping,
                                DMLOperator operator,
                                ResultWrapper<T, ?> wrapper,
                                ContextKeyValue<?>... keyValues) {
        super(tableMetadata, mapping, operator, wrapper,keyValues);
    }

    @Override
    public Flux<T> fetch() {
        return operator
                .query(tableName)
                .select(getSelectColumn())
                .where(param.getTerms())
                .orderBy(getSortOrder())
                .when(param.isPaging(), query -> query.paging(param.getPageIndex(), param.getPageSize()))
                .accept(queryOperator ->
                        tableMetadata.fireEvent(MappingEventTypes.select_before, eventContext ->
                                eventContext.set(
                                        source(DefaultReactiveQuery.this),
                                        query(queryOperator),
                                        tableMetadata(tableMetadata),
                                        dml(operator),
                                        executorType("reactive"),
                                        type("fetch")
                                )))
                .fetch(eventWrapper(tableMetadata, wrapper, executorType("reactive"), type("fetch")))
                .reactive();
    }

    @Override
    public Mono<T> fetchOne() {
        return  operator
                .query(tableName)
                .select(getSelectColumn())
                .where(param.getTerms())
                .orderBy(getSortOrder())
                .paging(0, 1)
                .accept(queryOperator ->
                        tableMetadata.fireEvent(MappingEventTypes.select_before, eventContext ->
                                eventContext.set(
                                        source(DefaultReactiveQuery.this),
                                        query(queryOperator),
                                        dml(operator),
                                        tableMetadata(tableMetadata),
                                        executorType("reactive"),
                                        type("fetchOne")
                                )))
                .fetch(eventWrapper(tableMetadata, wrapper, executorType("reactive"), type("fetchOne")))
                .reactive()
                .as(Mono::from);
    }

    @Override
    public Mono<Integer> count() {
        return operator
                .query(tableName)
                .select(count1().as("total"))
                .where(param.getTerms())
                .accept(queryOperator ->
                        tableMetadata.fireEvent(MappingEventTypes.select_before, eventContext ->
                                eventContext.set(
                                        source(DefaultReactiveQuery.this),
                                        query(queryOperator),
                                        dml(operator),
                                        tableMetadata(tableMetadata),
                                        executorType("reactive"),
                                        type("count")
                                )))
                .fetch(column("total", Number.class::cast))
                .reactive()
                .map(Number::intValue)
                .reduce(Math::addExact)
                .switchIfEmpty(Mono.just(0));
    }

}
