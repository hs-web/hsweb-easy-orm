package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import reactor.core.publisher.Flux;

public interface QueryResultOperator<E, R> extends ResultOperator<E, R> {
    @Override
    R sync();

    @Override
    Flux<E> reactive();
}
