package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.meta.Feature;
import reactor.core.publisher.Mono;

import java.util.function.Supplier;

/**
 * Optional reactive CRUD orchestration at the original-input boundary.
 *
 * <p>Register on entity table metadata. The lazy continuation retains normal ORM SQL and
 * lifecycle events. An interceptor can compose a transaction around it or use
 * {@link ReactiveCrudExecutionContext#executeVirtualUpdate(Supplier)} for lifecycle-aware
 * virtual-only updates without creating dummy physical writes. Implementations must compose, never detach
 * work, preserve permission predicates, and return the operation's declared result type.</p>
 *
 * @since 4.2.3
 * @see ReactiveCrudExecutionContext
 */
public interface ReactiveCrudExecutionInterceptor extends Feature {

    String ID_VALUE = "reactiveCrudExecutionInterceptor";
    FeatureId<ReactiveCrudExecutionInterceptor> ID = FeatureId.of(ID_VALUE);

    /**
     * Intercept one subscribed operation, before native input merging or SQL construction.
     *
     * @param context original input and private execution state
     * @param proceed lazy native execution; call at most once, selecting either native or virtual continuation for UPDATE
     * @param <R> Integer for insert/update/delete, SaveResult for save
     * @return the awaited operation result
     */
    <R> Mono<R> intercept(ReactiveCrudExecutionContext context, Supplier<Mono<R>> proceed);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return getType().getName();
    }

    @Override
    default FeatureType getType() {
        return MappingFeatureType.reactiveCrudExecutionInterceptor;
    }
}
