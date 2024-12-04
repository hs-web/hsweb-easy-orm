package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.apache.commons.collections4.CollectionUtils;
import org.hswebframework.ezorm.rdb.events.ContextKey;
import org.hswebframework.ezorm.rdb.events.ContextKeys;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.ReactiveDelete;
import org.hswebframework.ezorm.rdb.mapping.ReactiveQuery;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.ReactiveUpdate;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.context.Context;

import java.util.Collection;
import java.util.function.Function;
import java.util.function.Supplier;

public class DefaultReactiveRepository<E, K> extends DefaultRepository<E> implements ReactiveRepository<E, K> {

    private final Logger logger;

    public DefaultReactiveRepository(DatabaseOperator operator, String table, Class<E> type, ResultWrapper<E, ?> wrapper) {
        this(operator,
             () -> operator
                 .getMetadata()
                 .getTable(table)
                 .orElseThrow(() -> new UnsupportedOperationException("table [" + table + "] doesn't exist")), type, wrapper);
    }

    public DefaultReactiveRepository(DatabaseOperator operator, RDBTableMetadata table, Class<E> type, ResultWrapper<E, ?> wrapper) {
        this(operator, () -> table, type, wrapper);
    }

    public DefaultReactiveRepository(DatabaseOperator operator, Supplier<RDBTableMetadata> table, Class<E> type, ResultWrapper<E, ?> wrapper) {
        super(operator, table, wrapper);
        initMapping(type);
        this.logger = getLogger(type);
    }

    private static Logger getLogger(Class<?> type) {
        return org.slf4j.LoggerFactory.getLogger(type);
    }

    @Override
    public Mono<E> newInstance() {
        return Mono.fromSupplier(wrapper::newRowInstance);
    }

    @Override
    public E newInstanceNow() {
        return wrapper.newRowInstance();
    }

    @Override
    public Mono<E> findById(Mono<K> primaryKey) {
        return primaryKey
            .flatMap(k -> createQuery().where(getIdColumn(), k).fetchOne());
    }

    @Override
    public Flux<E> findById(Flux<K> key) {
        return key.collectList()
                  .filter(CollectionUtils::isNotEmpty)
                  .flatMapMany(idList -> createQuery().where().in(getIdColumn(), idList).fetch());
    }

    @Override
    public Mono<Integer> deleteById(Publisher<K> key) {
        return Flux.from(key)
                   .collectList()
                   .filter(CollectionUtils::isNotEmpty)
                   .flatMap(list -> createDelete().where().in(getIdColumn(), list).execute())
                   .defaultIfEmpty(0);
    }

    @Override
    public Mono<Integer> updateById(K id, Mono<E> data) {
        return data
            .flatMap(_data -> createUpdate()
                .where(getIdColumn(), id)
                .set(_data)
                .execute());
    }

    @Override
    public Mono<SaveResult> save(Publisher<E> data) {
        return Flux
            .from(data)
            .collectList()
            .filter(CollectionUtils::isNotEmpty)
            .flatMap(list -> doSave(list).reactive().contextWrite(this::applyContext))
            .defaultIfEmpty(SaveResult.of(0, 0));
    }

    @Override
    public Mono<Integer> insert(Publisher<E> data) {
        return Flux
            .from(data)
            .buffer(100)
            .as(this::insertBatch);
    }

    @Override
    public Mono<Integer> insertBatch(Publisher<? extends Collection<E>> data) {
        return Flux
            .from(data)
            .filter(CollectionUtils::isNotEmpty)
            .flatMap(e -> doInsert(e).reactive())
            .reduce(Math::addExact)
            .defaultIfEmpty(0)
            .contextWrite(this::applyContext);
    }

    @Override
    public ReactiveQuery<E> createQuery() {
        return new DefaultReactiveQuery<>(getTable()
            , mapping
            , operator.dml()
            , wrapper
            , this::applyContext
            , getDefaultContextKeyValue());
    }

    @Override
    public ReactiveUpdate<E> createUpdate() {
        return new DefaultReactiveUpdate<>(
            getTable()
            , operator.dml().update(getTable())
            , mapping
            , this::applyContext
            , getDefaultContextKeyValue());
    }

    @Override
    public ReactiveDelete createDelete() {
        return new DefaultReactiveDelete(getTable()
            , operator.dml().delete(getTable())
            , this::applyContext
            , getDefaultContextKeyValue()
        );
    }

    protected Context applyContext(Context context) {
        return context.put(Logger.class, logger);
    }

    @Override
    public QueryOperator nativeQuery() {
        return operator
            .dml()
            .query(getTable());
    }
}
