package org.hswebframework.ezorm.rdb.mapping;

import reactor.core.publisher.Mono;

public interface ReactiveUpdate<E> extends DSLUpdate<E, ReactiveUpdate<E>> {

    Mono<Integer> execute();
}
