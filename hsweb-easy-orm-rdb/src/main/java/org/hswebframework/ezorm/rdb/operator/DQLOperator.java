package org.hswebframework.ezorm.rdb.operator;

import reactor.core.publisher.Flux;

import java.util.concurrent.CompletionStage;

public interface DQLOperator<E, R> {

    R sync();

    CompletionStage<R> async();

    Flux<E> reactive();

}
