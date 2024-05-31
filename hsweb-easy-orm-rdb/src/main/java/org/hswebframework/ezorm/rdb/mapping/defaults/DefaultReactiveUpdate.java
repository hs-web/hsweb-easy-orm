package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.ReactiveUpdate;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;
import org.slf4j.Logger;
import reactor.core.publisher.Mono;
import reactor.util.context.Context;

import java.util.function.BiFunction;
import java.util.function.Function;

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
        return mapper.apply(this, doExecute()
            .reactive()
            .contextWrite(context));
    }

    @Override
    public ReactiveUpdate<E> onExecute(BiFunction<ReactiveUpdate<E>, Mono<Integer>, Mono<Integer>> consumer) {
        this.mapper = this.mapper.andThen((r) -> consumer.apply(this, r));
        return this;
    }


}
