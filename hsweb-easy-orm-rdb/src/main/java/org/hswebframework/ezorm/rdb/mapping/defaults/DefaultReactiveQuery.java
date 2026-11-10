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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
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
        return doFetch(operator.query(tableMetadata), operation,
            (query, resultContext) -> executeRows(query, resultContext, one));
    }

    private Flux<T> executeRows(QueryOperator query, ReactiveQueryResultContext resultContext, boolean one) {
        query.context(param.getContext()).select(getSelectColumn()).where(param.getTerms())
            .orderBy(getSortOrder()).when(param.isForUpdate(), QueryOperator::forUpdate);
        ReactiveQueryResultTransformer transformer = tableMetadata.getFeature(ReactiveQueryResultTransformer.ID).orElse(null);
        int batchSize = transformer == null ? 0 : transformer.getBatchSize(resultContext);
        if (batchSize > 0 && (one || !param.isPaging() || param.getPageSize() > 0)) {
            appendStableOrder(query);
            int size = one ? 1 : param.isPaging() ? Math.min(batchSize, param.getPageSize()) : batchSize;
            long offset = !one && param.isPaging() ? (long) param.getPageIndex() * param.getPageSize() : 0;
            long remaining = one ? 1 : param.isPaging() ? param.getPageSize() : Long.MAX_VALUE;
            return fetchBatches(query, resultContext, transformer, size,
                Math.toIntExact(offset / size), (int) (offset % size), remaining);
        }
        query.when(!one && param.isPaging(), operator -> operator.paging(param.getPageIndex(), param.getPageSize()));
        Flux<T> rows = readRows(query, resultContext.getOperation()).transform(source -> one ? source.take(1) : source);
        return transformer == null ? rows : transformer.transform(rows, resultContext);
    }

    private Flux<T> readRows(QueryOperator query, String operation) {
        return query.fetch(eventWrapper(tableMetadata, wrapper, executorType("reactive"), type(operation))).reactive();
    }

    private void appendStableOrder(QueryOperator query) {
        List<SortOrder> requested = Arrays.asList(getSortOrder());
        tableMetadata.getColumns().stream().filter(column -> column.isPrimaryKey()).forEach(column -> {
            String property = columnMapping.getPropertyByColumnName(column.getName()).orElse(column.getName());
            if (requested.stream().noneMatch(sort -> sort.getFunction() == null
                && (column.getName().equals(sort.getColumn()) || property.equals(sort.getColumn())))) {
                query.orderBy(SortOrder.asc(column.getName()));
            }
        });
    }

    private Flux<T> fetchBatches(QueryOperator query, ReactiveQueryResultContext context,
                                  ReactiveQueryResultTransformer transformer, int size, int page,
                                  int skip, long remaining) {
        // Collect only one SQL-sized segment. The Result completes before enrichment subscribes;
        // the next segment is deferred until this transformation completes or is cancelled.
        return readRows(query.clone().paging(page, size), context.getOperation()).collectList().flatMapMany(rows -> {
            if (rows.isEmpty()) {
                return Flux.empty();
            }
            int start = Math.min(skip, rows.size());
            int end = start + (int) Math.min(remaining, rows.size() - start);
            Flux<T> transformed = transformer.transform(Flux.fromIterable(rows.subList(start, end)), context);
            long nextRemaining = remaining - (end - start);
            return rows.size() < size || nextRemaining == 0 ? transformed
                : transformed.concatWith(Flux.defer(() -> fetchBatches(query, context, transformer, size,
                    Math.addExact(page, 1), 0, nextRemaining)));
        });
    }

    @Override
    public Mono<T> fetchOne() {
        Flux<T> rows = hasResultTransformer()
            ? Flux.defer(() -> executionCopy(true).fetchRows(true))
            : fetchRows(true);
        return rows.contextWrite(context).singleOrEmpty();
    }

    private <O> Flux<O> doFetch(QueryOperator queryOperator, String type,
                               BiFunction<QueryOperator, ReactiveQueryResultContext, Publisher<O>> executor) {
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
        return holder.doBefore().thenMany(Flux.defer(() -> Flux.from(executor.apply(queryOperator.clone(), resultContext))));
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
            .doFetch(queryOperator, "count", (_opt, ignored) -> _opt
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
            .doFetch(queryOperator, "count", (_opt, ignored) -> _opt
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
