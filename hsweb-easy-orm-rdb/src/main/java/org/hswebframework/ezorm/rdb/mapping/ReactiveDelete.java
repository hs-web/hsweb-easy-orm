package org.hswebframework.ezorm.rdb.mapping;

import reactor.core.publisher.Mono;

import java.util.function.BiFunction;
import java.util.function.Function;

public interface ReactiveDelete extends DSLDelete<ReactiveDelete> {
    Mono<Integer> execute();

    ReactiveDelete onExecute(BiFunction<ReactiveDelete, Mono<Integer>, Mono<Integer>> mapper);
}
