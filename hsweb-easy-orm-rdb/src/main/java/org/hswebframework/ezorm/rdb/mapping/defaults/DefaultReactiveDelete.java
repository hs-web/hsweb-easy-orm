package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.mapping.ReactiveDelete;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;
import org.slf4j.Logger;
import reactor.core.publisher.Mono;
import reactor.util.context.Context;

import java.util.function.BiFunction;
import java.util.function.Function;

public class DefaultReactiveDelete extends DefaultDelete<ReactiveDelete> implements ReactiveDelete {

    private final Function<Context, Context> context;
    public DefaultReactiveDelete(RDBTableMetadata tableMetadata,
                                 DeleteOperator operator,
                                 Function<Context, Context> context,
                                 ContextKeyValue<?>... keyValues) {
        super(tableMetadata, operator, keyValues);
        this.context = context;
    }

    public BiFunction<ReactiveDelete, Mono<Integer>, Mono<Integer>> mapper = (reactiveDelete, integerMono) -> integerMono;

    @Override
    public Mono<Integer> execute() {
        return mapper.apply(this, this
                .doExecute()
                .reactive()
                .contextWrite(context));
    }

    @Override
    public ReactiveDelete onExecute(BiFunction<ReactiveDelete, Mono<Integer>, Mono<Integer>> mapper) {
        this.mapper = this.mapper.andThen(r -> mapper.apply(this, r));
        return this;
    }
}
