package org.hswebframework.ezorm.rdb.mapping;

import reactor.core.publisher.Mono;

import java.util.function.Function;

public interface ReactiveDelete extends DSLDelete<ReactiveDelete> {
    Mono<Integer> execute();

    ReactiveDelete onExecute(Function<Mono<Integer>, Mono<Integer>> consumer);
}
