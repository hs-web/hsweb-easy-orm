package org.hswebframework.ezorm.rdb.executor.reactive;

import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.reactivestreams.Publisher;

public interface ReactiveResultWrapper<E,R,P extends Publisher<E>> extends ResultWrapper<E,R> {


}
