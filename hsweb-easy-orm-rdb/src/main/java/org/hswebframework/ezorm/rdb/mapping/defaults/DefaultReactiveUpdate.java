package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.ReactiveUpdate;
import org.hswebframework.ezorm.rdb.mapping.ReactiveCrudExecutionContext;
import org.hswebframework.ezorm.rdb.mapping.ReactiveCrudExecutionInterceptor;
import org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateResultOperator;
import reactor.core.publisher.Mono;
import reactor.util.context.Context;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.concurrent.atomic.AtomicBoolean;

public class DefaultReactiveUpdate<E> extends DefaultUpdate<E, ReactiveUpdate<E>> implements ReactiveUpdate<E> {

    private final Function<Context, Context> context;

    public DefaultReactiveUpdate(RDBTableMetadata table,
                                 UpdateOperator operator,
                                 EntityColumnMapping mapping,
                                 Function<Context, Context> context,
                                 ContextKeyValue<?>... keyValues) {
        super(table, operator, mapping, keyValues);
        this.context = context;
    }


    private BiFunction<ReactiveUpdate<E>, Mono<Integer>, Mono<Integer>> mapper = (update, mono) -> mono;

    @Override
    public Mono<Integer> execute() {
        if (!table.getFeature(ReactiveCrudExecutionInterceptor.ID).isPresent()) {
            return mapper.apply(this, doExecute().reactive().contextWrite(context));
        }
        return mapper.apply(this, Mono.defer(() -> {
                ReactiveCrudExecutionInterceptor interceptor = table.getFeatureNow(ReactiveCrudExecutionInterceptor.ID);
                DefaultReactiveUpdate<E> execution = executionCopy();
                AtomicBoolean continuationSubscribed = new AtomicBoolean();
                ReactiveCrudExecutionContext executionContext = new ReactiveCrudExecutionContext(
                    table, mapping, ReactiveCrudExecutionContext.Operation.UPDATE,
                    instance, execution.tempInstance, execution.toQueryParam(), execution,
                    Integer.class, execution::omitColumns,
                    mutation -> executeOnce(continuationSubscribed, () -> execution.doVirtualUpdate(mutation).reactive()));
                execution.contextKeyValues.add(MappingContextKeys.submittedValues.value(executionContext.getChangedValues()));
                return interceptor.intercept(executionContext,
                    () -> executeOnce(continuationSubscribed, () -> execution.doExecute().reactive()));
            }))
            .contextWrite(context);
    }

    private static Mono<Integer> executeOnce(AtomicBoolean subscribed, Supplier<Mono<Integer>> execution) {
        return Mono.defer(() -> {
            if (!subscribed.compareAndSet(false, true)) {
                return Mono.error(new IllegalStateException("only one native or virtual update continuation may execute"));
            }
            return execution.get();
        });
    }

    private UpdateResultOperator doVirtualUpdate(Supplier<Mono<Integer>> mutation) {
        return doExecute(() -> new UpdateResultOperator() {
            @Override
            public Integer sync() {
                throw new UnsupportedOperationException("virtual updates require reactive execution");
            }

            @Override
            public Mono<Integer> reactive() {
                return Mono.defer(mutation);
            }
        });
    }

    private DefaultReactiveUpdate<E> executionCopy() {
        DefaultReactiveUpdate<E> copy = new DefaultReactiveUpdate<>(table, operator.copy(), mapping, context);
        copy.terms = toQueryParam().clone().getTerms();
        copy.includes.addAll(includes);
        copy.excludes.addAll(excludes);
        copy.tempInstance.putAll(tempInstance);
        copy.instance = instance;
        for (ContextKeyValue<?> value : contextKeyValues) {
            String key = value.getKey();
            if (!key.equals(org.hswebframework.ezorm.rdb.events.ContextKeys.source.getKey())
                && !key.equals(MappingContextKeys.update.getKey())
                && !key.equals(MappingContextKeys.updateColumnInstance.getKey())) {
                copy.contextKeyValues.add(value);
            }
        }
        return copy;
    }

    @Override
    public ReactiveUpdate<E> onExecute(BiFunction<ReactiveUpdate<E>, Mono<Integer>, Mono<Integer>> consumer) {
        this.mapper = this.mapper.andThen((r) -> consumer.apply(this, r));
        return this;
    }


}
