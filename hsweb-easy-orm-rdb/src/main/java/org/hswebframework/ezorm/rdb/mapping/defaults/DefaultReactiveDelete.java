package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.mapping.ReactiveDelete;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;
import org.slf4j.Logger;
import reactor.core.publisher.Mono;

import java.util.function.BiFunction;

public class DefaultReactiveDelete extends DefaultDelete<ReactiveDelete> implements ReactiveDelete {

    private final Logger logger;

    public DefaultReactiveDelete(RDBTableMetadata tableMetadata,
                                 DeleteOperator operator,
                                 Logger logger,
                                 ContextKeyValue<?>... keyValues) {
        super(tableMetadata, operator, keyValues);
        this.logger = logger;
    }

    public BiFunction<ReactiveDelete, Mono<Integer>, Mono<Integer>> mapper = (reactiveDelete, integerMono) -> integerMono;

    @Override
    public Mono<Integer> execute() {
        return mapper.apply(this, this
                .doExecute()
                .reactive()
                .contextWrite(ctx -> ctx.put(Logger.class, logger)));
    }

    @Override
    public ReactiveDelete onExecute(BiFunction<ReactiveDelete, Mono<Integer>, Mono<Integer>> mapper) {
        this.mapper = this.mapper.andThen(r -> mapper.apply(this, r));
        return this;
    }
}
