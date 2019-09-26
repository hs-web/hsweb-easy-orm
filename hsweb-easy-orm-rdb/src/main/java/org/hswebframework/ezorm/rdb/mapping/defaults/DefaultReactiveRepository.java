package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.core.CastUtil;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.ReactiveDelete;
import org.hswebframework.ezorm.rdb.mapping.ReactiveQuery;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.ReactiveUpdate;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Collection;

public class DefaultReactiveRepository<E, K> extends DefaultRepository<E> implements ReactiveRepository<E, K> {
    public DefaultReactiveRepository(DatabaseOperator operator, RDBTableMetadata table, Class<E> type, ResultWrapper<E, ?> wrapper) {
        super(operator, table, wrapper);
        initMapping(type);
    }

    @Override
    public Mono<E> newInstance() {
        return Mono.just(wrapper.newRowInstance());
    }

    @Override
    public Mono<E> findById(Mono<K> primaryKey) {
        return primaryKey
                .flatMap(k -> createQuery().where(idColumn, k).fetchOne());
    }

    @Override
    public Mono<Integer> deleteById(Publisher<K> key) {
        return Flux.from(key)
                .collectList()
                .flatMap(list -> createDelete().where().in(idColumn, list).execute());
    }

    @Override
    public Mono<SaveResult> save(Publisher<E> data) {
        // TODO: 2019-09-24 还应该按照唯一约束来处理
        return Flux
                .from(data)
                .flatMap(e -> getPropertyOperator()
                        .getProperty(e, idColumn)
                        .map(CastUtil::<K>cast)
                        .map(primaryKey ->
                                createUpdate()
                                        .set(e)
                                        .where(idColumn, primaryKey)
                                        .execute()
                                        .flatMap(i -> {
                                            if (i >= 0) {
                                                return Mono.just(SaveResult.of(0, i));
                                            }
                                            return insert(Mono.just(e))
                                                    .map(j -> SaveResult.of(j, 0));
                                        }))
                        .orElseGet(() ->
                                insert(Mono.just(e)).map(i -> SaveResult.of(i, 0))
                        ))
                .reduce(SaveResult.of(0, 0), SaveResult::merge);
    }

    @Override
    public Mono<Integer> insert(Publisher<E> data) {
        return Flux.from(data)
                .flatMap(e -> doInsert(e).reactive().flux())
                .reduce(Math::addExact);
    }

    @Override
    public Mono<Integer> insertBatch(Publisher<? extends Collection<E>> data) {
        return Flux.from(data)
                .flatMap(e -> doInsert(e).reactive().flux())
                .reduce(Math::addExact);
    }

    @Override
    public ReactiveQuery<E> createQuery() {
        return new DefaultReactiveQuery<>(table, mapping, operator.dml(), wrapper);
    }

    @Override
    public ReactiveUpdate<E> createUpdate() {
        return new DefaultReactiveUpdate<>(table, operator.dml().update(table.getFullName()), mapping);
    }

    @Override
    public ReactiveDelete createDelete() {
        return new DefaultReactiveDelete(table, operator.dml().delete(table.getFullName()));
    }
}
