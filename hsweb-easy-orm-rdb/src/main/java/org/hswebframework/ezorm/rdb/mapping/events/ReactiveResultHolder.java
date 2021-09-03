package org.hswebframework.ezorm.rdb.mapping.events;

import reactor.core.publisher.Mono;

import java.util.function.Function;

public interface ReactiveResultHolder {

    void after(Function<Object,Mono<Void>> listener);

    void before(Mono<Void> listener);

}
