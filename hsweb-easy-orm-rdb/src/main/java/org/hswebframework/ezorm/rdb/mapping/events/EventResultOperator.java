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
import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.result;

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
        return (T) Proxy
                .newProxyInstance(
                        EventResultOperator.class.getClassLoader(),
                        new Class[]{target},
                        ((proxy, method, args) -> {

                            ContextKeyValue<Boolean> isReactive = MappingContextKeys
                                    .reactive(Publisher.class.isAssignableFrom(method.getReturnType()));
                            try {

                                DefaultReactiveResultHolder holder = new DefaultReactiveResultHolder();

                                tableMetadata.fireEvent(before, ctx -> {
                                    ctx.set(keyValue).set(isReactive, reactiveResult(holder));
                                });

                                if (!isReactive.getValue()) {
                                    Object result = method.invoke(operator.get(), args);
                                    tableMetadata.fireEvent(after, ctx -> {
                                        ctx.set(keyValue).set(MappingContextKeys.result(result), isReactive);
                                    });
                                    return result;
                                }
                                boolean isMono = Mono.class.isAssignableFrom(method.getReturnType());
                                return holder
                                        .doBefore()
                                        .then(Mono.fromCallable(() -> method.invoke(operator.get(), args)))
                                        .flatMapMany(result -> {
                                            return holder
                                                    .doInvoke()
                                                    .thenMany((Publisher<Object>) result)
                                                    //有返回值
                                                    .map(holder::doAfter)
                                                    //无返回值
                                                    .defaultIfEmpty(holder.doAfterNoResult())
                                                    .flatMap(Function.identity());
                                        })
                                        .as(isMono ? Mono::from : flux -> flux)
                                        ;

                            } catch (Throwable e) {
                                tableMetadata.fireEvent(after, ctx -> {
                                    ctx.set(keyValue).set(MappingContextKeys.error(e), isReactive);
                                });
                                throw e;
                            }
                        }));
    }
}
