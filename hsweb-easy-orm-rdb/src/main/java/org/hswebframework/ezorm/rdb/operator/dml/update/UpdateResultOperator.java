package org.hswebframework.ezorm.rdb.operator.dml.update;

import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import reactor.core.publisher.Mono;


public interface UpdateResultOperator extends ResultOperator<Integer, Integer> {
    @Override
    Integer sync();

    @Override
    Mono<Integer> reactive();
}
