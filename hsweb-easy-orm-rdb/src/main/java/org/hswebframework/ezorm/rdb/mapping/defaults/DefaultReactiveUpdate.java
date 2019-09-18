package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.mapping.ReactiveUpdate;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;
import reactor.core.publisher.Mono;

public class DefaultReactiveUpdate<E> extends DefaultUpdate<E, ReactiveUpdate<E>> implements ReactiveUpdate<E> {

    public DefaultReactiveUpdate(RDBTableMetadata table, UpdateOperator operator, Class<E> entityType) {
        super(table, operator, entityType);
    }

    @Override
    public Mono<Integer> execute() {
        return doExecute().reactive();
    }
}
