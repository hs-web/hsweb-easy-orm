package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.core.param.Sort;
import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.ReactiveQuery;
import org.hswebframework.ezorm.rdb.mapping.ReactiveQueryResultContext;
import org.hswebframework.ezorm.rdb.mapping.ReactiveQueryResultTransformer;
import org.hswebframework.ezorm.rdb.mapping.events.DefaultReactiveResultHolder;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.DMLOperator;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.context.Context;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.function.Function;

import static org.hswebframework.ezorm.rdb.events.ContextKeys.source;
import static org.hswebframework.ezorm.rdb.events.ContextKeys.tableMetadata;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;
import static org.hswebframework.ezorm.rdb.mapping.events.EventSupportWrapper.*;
import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.*;
import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.type;
import static org.hswebframework.ezorm.rdb.operator.dml.query.Selects.count1;

public class DefaultReactiveQuery<T> extends DefaultQuery<T, ReactiveQuery<T>> implements ReactiveQuery<T> {

    private final Function<Context, Context> context;

    public DefaultReactiveQuery(TableOrViewMetadata tableMetadata,
                                EntityColumnMapping mapping,
                                DMLOperator operator,
                                ResultWrapper<T, ?> wrapper,
                                Function<Context, Context> context,
                                ContextKeyValue<?>... keyValues) {
        super(tableMetadata, mapping, operator, wrapper, keyValues);
        this.context = context;
    }

    @Override
    public ReactiveQuery<T> copy() {
        return executionCopy(false);
    }

    private DefaultReactiveQuery<T> executionCopy(boolean normalizeOrders) {
        DefaultReactiveQuery<T> copy = new DefaultReactiveQuery<>(
            tableMetadata, columnMapping, operator, wrapper, context);
        copy.param = param.clone();
        copy.param.setSorts(new ArrayList<>());
        for (Sort sort : param.getSorts()) {
            Sort snapshot = new Sort(sort.getName());
            snapshot.setOrder(sort.getOrder());
            snapshot.setType(sort.getType());
            snapshot.setValue(sort.getValue());
            snapshot.setOpts(sort.getOpts() == null ? null : new HashMap<>(sort.getOpts()));
            copy.param.getSorts().add(snapshot);
        }
        for (SortOrder sort : orders) {
            if (normalizeOrders) {
                // Preparation and native SQL must use the same subscription-private sort list.
                // Keep parameter sorts first, matching DefaultQuery.getSortOrder() precedence.
                Sort snapshot = new Sort(sort.getColumn());
                snapshot.setOrder(sort.getOrder().name());
                snapshot.setType(sort.getFunction());
                snapshot.setValue(sort.getValue());
                snapshot.setOpts(sort.getOpts() == null ? null : new HashMap<>(sort.getOpts()));
                copy.param.getSorts().add(snapshot);
            } else {
                SortOrder snapshot = new SortOrder();
                snapshot.setColumn(sort.getColumn());
                snapshot.setOrder(sort.getOrder());
                snapshot.setFunction(sort.getFunction());
                snapshot.setValue(sort.getValue());
                snapshot.setOpts(sort.getOpts() == null ? null : new HashMap<>(sort.getOpts()));
                copy.orders.add(snapshot);
            }
        }
        return copy;
    }

    @Override
    public Flux<T> fetch() {
        // Preparation listeners and transformer state must never leak between subscribers.
        Flux<T> rows = hasResultTransformer()
            ? Flux.defer(() -> executionCopy(true).fetchRows(false))
            : fetchRows(false);
        return rows.contextWrite(context);
    }

    private boolean hasResultTransformer() {
        return tableMetadata.getFeature(ReactiveQueryResultTransformer.ID).isPresent();
    }

    private Flux<T> fetchRows(boolean one) {
        String operation = one ? "fetchOne" : "fetch";
        return this
            .doFetch(operator.query(tableMetadata),
                     operation,
                     (_queryOperator) -> _queryOperator
                         .context(param.getContext())
                         .select(getSelectColumn())
                         .where(param.getTerms())
                         .orderBy(getSortOrder())
                         .when(!one && param.isPaging(), query -> query.paging(param.getPageIndex(), param.getPageSize()))
                         .when(param.isForUpdate(), QueryOperator::forUpdate)
                         .fetch(eventWrapper(tableMetadata, wrapper, executorType("reactive"), type(operation)))
                         .reactive()
                         .transform(rows -> one ? rows.take(1) : rows));
    }

    @Override
    public Mono<T> fetchOne() {
        Flux<T> rows = hasResultTransformer()
            ? Flux.defer(() -> executionCopy(true).fetchRows(true))
            : fetchRows(true);
        return rows.contextWrite(context).singleOrEmpty();
    }

    private <O> Flux<O> doFetch(QueryOperator queryOperator, String type, Function<QueryOperator, Publisher<O>> executor) {
        DefaultReactiveResultHolder holder = new DefaultReactiveResultHolder();
        ReactiveQueryResultContext resultContext = new ReactiveQueryResultContext(
            tableMetadata, columnMapping, type, param);
        tableMetadata
            .fireEvent(MappingEventTypes.select_before, eventContext ->
                eventContext.set(
                    source(DefaultReactiveQuery.this),
                    query(queryOperator),
                    dml(operator),
                    tableMetadata(tableMetadata),
                    columnMapping(columnMapping),
                    reactiveResultHolder.value(holder),
                    reactiveQueryResultContext.value(resultContext),
                    queryOaram.value(param),
                    executorType("reactive"),
                    type(type)
                ));
        return holder.doBefore().thenMany(Flux.defer(() -> {
            Flux<O> rows = Flux.from(executor.apply(queryOperator.clone()));
            if (!"fetch".equals(type) && !"fetchOne".equals(type)) {
                return rows;
            }
            // Count paths share async preparation but never expose aggregate values to this SPI.
            return tableMetadata.getFeature(ReactiveQueryResultTransformer.ID)
                                .map(transformer -> transformer.transform(rows, resultContext))
                                .orElse(rows);
        }));
    }

    @Override
    public Mono<Integer> count() {
        Mono<Integer> result = hasResultTransformer()
            ? Mono.defer(() -> executionCopy(true).countRows())
            : countRows();
        return result.contextWrite(context);
    }

    private Mono<Integer> countRows() {
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
            .singleOrEmpty();
    }

    @Override
    public Mono<Long> countLong() {
        Mono<Long> result = hasResultTransformer()
            ? Mono.defer(() -> executionCopy(true).countLongRows())
            : countLongRows();
        return result.contextWrite(context);
    }

    private Mono<Long> countLongRows() {
        QueryOperator queryOperator = operator
            .query(tableMetadata)
            .select(count1().as("_total"));
        return this
            .doFetch(queryOperator, "count", _opt -> _opt
                .context(param.getContext())
                .where(param.getTerms())
                .fetch(column("_total", Number.class::cast))
                .reactive()
                .map(Number::longValue)
                .reduce(Math::addExact)
                .switchIfEmpty(Mono.just(0L)))
            .singleOrEmpty();
    }
}
