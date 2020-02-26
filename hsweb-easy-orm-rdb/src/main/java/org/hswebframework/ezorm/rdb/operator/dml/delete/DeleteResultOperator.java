package org.hswebframework.ezorm.rdb.operator.dml.delete;

import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import reactor.core.publisher.Mono;


public interface DeleteResultOperator extends ResultOperator<Integer, Integer> {
    @Override
    Integer sync();

    @Override
    Mono<Integer> reactive();
}
