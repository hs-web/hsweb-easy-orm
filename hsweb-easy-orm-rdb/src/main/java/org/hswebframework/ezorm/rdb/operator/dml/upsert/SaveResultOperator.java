package org.hswebframework.ezorm.rdb.operator.dml.upsert;

import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import reactor.core.publisher.Mono;

public interface SaveResultOperator extends ResultOperator<SaveResult, SaveResult> {
    @Override
    SaveResult sync();

    @Override
    Mono<SaveResult> reactive();
}
