package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.mapping.ReactiveDelete;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.ReactiveCrudExecutionContext;
import org.hswebframework.ezorm.rdb.mapping.ReactiveCrudExecutionInterceptor;
import org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;
import reactor.core.publisher.Mono;
import reactor.util.context.Context;

import java.util.function.BiFunction;
import java.util.function.Function;

public class DefaultReactiveDelete extends DefaultDelete<ReactiveDelete> implements ReactiveDelete {

    private final Function<Context, Context> context;
    private final RDBTableMetadata tableMetadata;
    public DefaultReactiveDelete(RDBTableMetadata tableMetadata,
                                 DeleteOperator operator,
                                 Function<Context, Context> context,
                                 ContextKeyValue<?>... keyValues) {
        super(tableMetadata, operator, keyValues);
        this.context = context;
        this.tableMetadata = tableMetadata;
    }

    public BiFunction<ReactiveDelete, Mono<Integer>, Mono<Integer>> mapper = (reactiveDelete, integerMono) -> integerMono;

    @Override
    public Mono<Integer> execute() {
        if (!tableMetadata.getFeature(ReactiveCrudExecutionInterceptor.ID).isPresent()) {
            return mapper.apply(this, doExecute().reactive().contextWrite(context));
        }
        return mapper.apply(this, Mono.defer(() -> {
                ReactiveCrudExecutionInterceptor interceptor = tableMetadata.getFeatureNow(ReactiveCrudExecutionInterceptor.ID);
                DefaultReactiveDelete execution = executionCopy();
                EntityColumnMapping mapping = null;
                for (ContextKeyValue<?> value : execution.contextKeyValues) {
                    if (value.getKey().equals(MappingContextKeys.columnMapping.getKey())) {
                        mapping = (EntityColumnMapping) value.getValue();
                    }
                }
                ReactiveCrudExecutionContext executionContext = new ReactiveCrudExecutionContext(
                    tableMetadata, mapping, ReactiveCrudExecutionContext.Operation.DELETE,
                    null, null, execution.toQueryParam(), execution, Integer.class, null);
                return interceptor.intercept(executionContext, () -> execution.doExecute().reactive());
            }))
            .contextWrite(context);
    }

    private DefaultReactiveDelete executionCopy() {
        DefaultReactiveDelete copy = new DefaultReactiveDelete(tableMetadata, operator.copy(), context);
        copy.terms = toQueryParam().clone().getTerms();
        for (ContextKeyValue<?> value : contextKeyValues) {
            String key = value.getKey();
            if (!key.equals(org.hswebframework.ezorm.rdb.events.ContextKeys.source.getKey())
                && !key.equals(MappingContextKeys.delete.getKey())) {
                copy.contextKeyValues.add(value);
            }
        }
        return copy;
    }

    @Override
    public ReactiveDelete onExecute(BiFunction<ReactiveDelete, Mono<Integer>, Mono<Integer>> mapper) {
        this.mapper = this.mapper.andThen(r -> mapper.apply(this, r));
        return this;
    }
}
