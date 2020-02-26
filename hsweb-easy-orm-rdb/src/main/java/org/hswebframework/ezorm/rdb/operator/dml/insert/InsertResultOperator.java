package org.hswebframework.ezorm.rdb.operator.dml.insert;

import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import reactor.core.publisher.Mono;


public interface InsertResultOperator extends ResultOperator<Integer, Integer> {
    @Override
    Integer sync();

    @Override
    Mono<Integer> reactive();
}
