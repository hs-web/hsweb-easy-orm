package org.hswebframework.ezorm.core.mapping;

import org.hswebframework.ezorm.core.Conditional;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface ReactiveQuery<T> extends Conditional<ReactiveQuery<T>> {


    ReactiveQuery<T> from(Object entityOrQueryParam);


    Flux<T> fetch();

    Mono<Integer> count();
}
