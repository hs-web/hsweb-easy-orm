package org.hswebframework.ezorm.rdb.mapping.events;

import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.events.EventListener;
import org.hswebframework.ezorm.rdb.events.EventType;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.*;

public class EventResultOperator {

    @SuppressWarnings("all")
    public static <T extends ResultOperator> T create(Supplier<T> operator,
                                                      Class<T> target,
                                                      RDBTableMetadata tableMetadata,
                                                      EventType before,
                                                      EventType after,
                                                      ContextKeyValue<?>... keyValue) {
        if (!tableMetadata.findFeature(EventListener.ID).isPresent()) {
            return operator.get();
        }
        return (T) Proxy.newProxyInstance(EventResultOperator.class.getClassLoader(),
                new Class[]{target},
                ((proxy, method, args) -> {
                    ContextKeyValue<Boolean> isReactive = MappingContextKeys.reactive(Publisher.class.isAssignableFrom(method.getReturnType()));
                    try {

                        List<Function<Object, Mono<Void>>> afters = new ArrayList<>();
                        List<Mono<Void>> befores = new ArrayList<>();
                        ReactiveResultHolder holder = new ReactiveResultHolder() {
                            @Override
                            public void after(Function<Object, Mono<Void>> result) {
                                afters.add(result);
                            }

                            @Override
                            public void before(Mono<Void> listener) {
                                befores.add(listener);
                            }
                        };
                        tableMetadata.fireEvent(before, ctx -> {
                            ctx.set(keyValue).set(isReactive, reactiveResult(holder));
                        });
                        Object result = method.invoke(operator.get(), args);

                        if (result instanceof Flux) {
                            return Flux.concat(befores)
                                    .thenMany(Flux
                                            .from((Publisher<?>) result)
                                            .concatMap(r -> {
                                                return Flux.concat(afters.stream()
                                                        .map(func -> func.apply(r).thenReturn(r))
                                                        .collect(Collectors.toList()));

                                            }).doOnComplete(() -> {
                                                tableMetadata.fireEvent(after, ctx -> ctx.set(keyValue).set(isReactive));
                                            })
                                            .doOnError(err -> {
                                                tableMetadata.fireEvent(after, ctx -> ctx.set(keyValue).set(isReactive, error(err)));
                                            }));


                        } else if (result instanceof Mono) {
                            return Flux.concat(befores)
                                    .then(Mono
                                            .from((Publisher<?>) result)
                                            .flatMap(r -> {
                                                return Flux.concat(afters.stream()
                                                        .map(func -> func.apply(r).thenReturn(r))
                                                        .collect(Collectors.toList()))
                                                        .then(Mono.just(r));
                                            })
                                            .doOnSuccess(r -> {
                                                tableMetadata
                                                        .fireEvent(after, ctx -> ctx.set(keyValue)
                                                                .set(isReactive, result(r)));
                                            })
                                            .doOnError(err -> {
                                                tableMetadata.fireEvent(after, ctx -> {
                                                    ctx.set(keyValue).set(isReactive, error(err));
                                                });
                                            }));
                        } else {
                            tableMetadata
                                    .fireEvent(after, ctx -> ctx.set(keyValue)
                                            .set(isReactive, result(result)));
                        }
                        return result;
                    } catch (Throwable e) {
                        tableMetadata.fireEvent(after, ctx -> {
                            ctx.set(keyValue).set(MappingContextKeys.error(e), isReactive);
                        });
                        throw e;
                    }
                }));
    }
}
