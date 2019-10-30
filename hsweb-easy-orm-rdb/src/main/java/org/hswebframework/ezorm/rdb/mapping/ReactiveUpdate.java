package org.hswebframework.ezorm.rdb.mapping;

import reactor.core.publisher.Mono;

import java.util.function.Function;

public interface ReactiveUpdate<E> extends DSLUpdate<E, ReactiveUpdate<E>> {

    Mono<Integer> execute();

    ReactiveUpdate<E> onExecute(Function<Mono<Integer>, Mono<Integer>>  consumer);
}
