package org.hswebframework.ezorm.rdb.operator.ddl;

import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import reactor.core.publisher.Mono;

public interface TableDDLResultOperator extends ResultOperator<Boolean, Boolean> {

    @Override
    Boolean sync();

    @Override
    Mono<Boolean> reactive();

}
