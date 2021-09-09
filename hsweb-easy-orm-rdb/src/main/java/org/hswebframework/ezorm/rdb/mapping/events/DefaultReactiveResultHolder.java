package org.hswebframework.ezorm.rdb.mapping.events;

import lombok.Getter;
import org.apache.commons.collections4.CollectionUtils;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

@Getter
public class DefaultReactiveResultHolder implements ReactiveResultHolder {
    private List<Function<Object, Mono<Void>>> afterListener;
    private List<Mono<Void>> beforeListener;
    private List<Mono<Void>> invokeListener;

    public Mono<Object> doAfterNoResult() {
        return doAfter(null);
    }

    public Mono<Object> doAfter(Object val) {
        Mono<Object> result = val == null ? Mono.empty() : Mono.just(val);

        if (CollectionUtils.isNotEmpty(afterListener)) {
            return Flux
                    .fromIterable(afterListener)
                    .concatMap(func -> func.apply(val))
                    .then(result);
        }
        return result;
    }

    public Mono<Void> doBefore() {
        if (CollectionUtils.isNotEmpty(beforeListener)) {
            return Flux
                    .concat(beforeListener)
                    .then();
        }
        return Mono.empty();
    }

    public Mono<Void> doInvoke() {
        if (CollectionUtils.isNotEmpty(invokeListener)) {
            return Flux
                    .concat(invokeListener)
                    .then();
        }
        return Mono.empty();
    }

    @Override
    public synchronized void after(Function<Object, Mono<Void>> listener) {
        if (afterListener == null) {
            afterListener = new ArrayList<>();
        }
        afterListener.add(listener);
    }

    @Override
    public synchronized void before(Mono<Void> listener) {
        if (beforeListener == null) {
            beforeListener = new ArrayList<>();
        }
        beforeListener.add(listener);
    }

    @Override
    public synchronized void invoke(Mono<Void> listener) {
        if (invokeListener == null) {
            invokeListener = new ArrayList<>();
        }
        invokeListener.add(listener);
    }
}
