package org.hswebframework.ezorm.rdb.operator;

import org.reactivestreams.Publisher;

import java.util.concurrent.CompletionStage;

public interface ResultOperator<E, R> {

    default R block(){
        return sync();
    }

    R sync();

    Publisher<E> reactive();

}
