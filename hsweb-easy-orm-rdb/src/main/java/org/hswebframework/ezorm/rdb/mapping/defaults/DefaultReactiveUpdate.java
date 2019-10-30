package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.ReactiveUpdate;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.util.function.Consumer;
import java.util.function.Function;

public class DefaultReactiveUpdate<E> extends DefaultUpdate<E, ReactiveUpdate<E>> implements ReactiveUpdate<E> {

    public DefaultReactiveUpdate(RDBTableMetadata table, UpdateOperator operator, EntityColumnMapping mapping) {
        super(table, operator, mapping);
    }

    private Consumer<SignalType> consumer = (s) -> {
    };

    private Function<Mono<Integer>, Mono<Integer>> before = Function.identity();

    @Override
    public Mono<Integer> execute() {
        return before.apply(doExecute()
                .reactive()
                .doFinally(consumer));
    }

    @Override
    public ReactiveUpdate<E> onExecute(Function<Mono<Integer>, Mono<Integer>> consumer) {
        this.before = this.before.andThen(consumer);
        return this;
    }


}
