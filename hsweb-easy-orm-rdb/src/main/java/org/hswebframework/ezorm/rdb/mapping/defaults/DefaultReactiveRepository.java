package org.hswebframework.ezorm.rdb.mapping.defaults;

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

public class DefaultReactiveRepository<E, PK> extends DefaultRepository<E> implements ReactiveRepository<E, PK> {
    public DefaultReactiveRepository(DatabaseOperator operator, RDBTableMetadata table, Class<E> type, ResultWrapper<E, ?> wrapper) {
        super(operator, table, type, wrapper);
    }

    @Override
    public Mono<E> findById(Mono<PK> primaryKey) {
        return primaryKey
                .flatMap(pk-> createQuery().where(idColumn, pk).fetchOne());
    }

    @Override
    public Mono<Integer> insert(Publisher<E> data) {
       return Flux.from(data)
               .flatMap(e -> doInsert(e).reactive().flux())
               .reduce(Math::addExact);
    }

    @Override
    public Mono<Integer> insertBatch(Publisher<Collection<E>> data) {
        return Flux.from(data)
                .flatMap(e -> doInsert(e).reactive().flux())
                .reduce(Math::addExact);
    }

    @Override
    public ReactiveQuery<E> createQuery() {
        return new DefaultReactiveQuery<>(table, entityType, operator.dml(), wrapper);
    }

    @Override
    public ReactiveUpdate<E> createUpdate() {
        return new DefaultReactiveUpdate<>(table, operator.dml().update(table.getFullName()), entityType);
    }

    @Override
    public ReactiveDelete createDelete() {
        return new DefaultReactiveDelete(operator.dml().delete(table.getFullName()));
    }
}
