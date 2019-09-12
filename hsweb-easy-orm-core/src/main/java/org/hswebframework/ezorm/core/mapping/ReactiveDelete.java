package org.hswebframework.ezorm.core.mapping;

import org.hswebframework.ezorm.core.Conditional;
import reactor.core.publisher.Mono;

public interface ReactiveDelete extends Conditional<ReactiveDelete> {
    Mono<Integer> execute();
}
