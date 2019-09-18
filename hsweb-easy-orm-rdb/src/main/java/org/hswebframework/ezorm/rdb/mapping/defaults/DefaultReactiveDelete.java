package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.mapping.ReactiveDelete;
import org.hswebframework.ezorm.rdb.mapping.ReactiveQuery;
import org.hswebframework.ezorm.rdb.mapping.SyncDelete;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;
import reactor.core.publisher.Mono;

public class DefaultReactiveDelete extends DefaultDelete<ReactiveDelete> implements ReactiveDelete {
    public DefaultReactiveDelete(DeleteOperator operator) {
        super(operator);
    }

    @Override
    public Mono<Integer> execute() {
        return doExecute().reactive();
    }
}
