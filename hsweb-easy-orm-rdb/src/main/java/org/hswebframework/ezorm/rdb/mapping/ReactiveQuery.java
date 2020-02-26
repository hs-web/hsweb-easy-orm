package org.hswebframework.ezorm.rdb.mapping;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface ReactiveQuery<T> extends DSLQuery<ReactiveQuery<T>> {

    Flux<T> fetch();

    Mono<Integer> count();

    Mono<T> fetchOne();
}
