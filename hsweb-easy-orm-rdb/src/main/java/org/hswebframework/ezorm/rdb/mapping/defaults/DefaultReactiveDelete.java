package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.mapping.ReactiveDelete;
import org.hswebframework.ezorm.rdb.mapping.ReactiveQuery;
import org.hswebframework.ezorm.rdb.mapping.SyncDelete;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;
import reactor.core.publisher.Mono;

import java.util.function.Function;

public class DefaultReactiveDelete extends DefaultDelete<ReactiveDelete> implements ReactiveDelete {
    public DefaultReactiveDelete(RDBTableMetadata tableMetadata, DeleteOperator operator) {
        super(tableMetadata, operator);
    }

    public Function<Mono<Integer>, Mono<Integer>> mapper = Function.identity();

    @Override
    public Mono<Integer> execute() {
        return mapper.apply(doExecute().reactive());
    }

    @Override
    public ReactiveDelete onExecute(Function<Mono<Integer>, Mono<Integer>> mapper) {
        this.mapper = mapper.andThen(mapper);
        return this;
    }
}
