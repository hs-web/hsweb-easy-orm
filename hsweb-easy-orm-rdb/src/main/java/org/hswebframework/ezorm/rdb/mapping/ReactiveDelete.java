package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.Conditional;
import reactor.core.publisher.Mono;

public interface ReactiveDelete extends DSLDelete<ReactiveDelete> {
    Mono<Integer> execute();
}
