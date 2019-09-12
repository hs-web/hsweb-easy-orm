package org.hswebframework.ezorm.core.mapping;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import reactor.core.publisher.Mono;

public interface ReactiveUpdate<E> extends Conditional<ReactiveUpdate<E>> {

    SyncUpdate set(E entity);

    SyncUpdate set(String column, Object value);

    default SyncUpdate set(MethodReferenceColumn<E> columnAndValue) {
        return set(columnAndValue.getColumn(), columnAndValue.get());
    }

    default SyncUpdate set(StaticMethodReferenceColumn<E> column, Object value) {
        return set(column.getColumn(), value);
    }

    Mono<Integer> execute();
}
