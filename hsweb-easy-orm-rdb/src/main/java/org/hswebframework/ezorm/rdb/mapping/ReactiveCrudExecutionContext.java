package org.hswebframework.ezorm.rdb.mapping;

import lombok.AccessLevel;
import lombok.Getter;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.rdb.events.EventContext;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Subscription-local CRUD input before duplicate merging or physical-column SQL generation.
 *
 * <p>Collection input retains original order and entity references, before repository duplicate
 * merging. Changed values are an immutable snapshot. Query conditions are private to the
 * execution; the interceptor must retain the caller's existing permission constraints.</p>
 *
 * @since 4.2.3
 * @see ReactiveCrudExecutionInterceptor
 */
@Getter
public final class ReactiveCrudExecutionContext {

    public enum Operation { INSERT, SAVE, UPDATE, DELETE }

    private final RDBTableMetadata tableMetadata;
    private final EntityColumnMapping columnMapping;
    private final Operation operation;
    private final Object input;
    private final Map<String, Object> changedValues;
    private final QueryParam queryParam;
    private final Object source;
    private final Class<?> resultType;
    private final EventContext attributes = EventContext.create();
    private final Consumer<String[]> columnOmission;
    @Getter(AccessLevel.NONE)
    private final Function<Supplier<Mono<Integer>>, Mono<Integer>> virtualUpdate;

    public ReactiveCrudExecutionContext(RDBTableMetadata tableMetadata,
                                        EntityColumnMapping columnMapping,
                                        Operation operation,
                                        Object input,
                                        Map<String, Object> changedValues,
                                        QueryParam queryParam,
                                        Object source,
                                        Class<?> resultType,
                                        Consumer<String[]> columnOmission) {
        this(tableMetadata, columnMapping, operation, input, changedValues, queryParam,
             source, resultType, columnOmission, null);
    }

    public ReactiveCrudExecutionContext(RDBTableMetadata tableMetadata,
                                        EntityColumnMapping columnMapping,
                                        Operation operation,
                                        Object input,
                                        Map<String, Object> changedValues,
                                        QueryParam queryParam,
                                        Object source,
                                        Class<?> resultType,
                                        Consumer<String[]> columnOmission,
                                        Function<Supplier<Mono<Integer>>, Mono<Integer>> virtualUpdate) {
        this.tableMetadata = tableMetadata;
        this.columnMapping = columnMapping;
        this.operation = operation;
        this.input = input instanceof Collection
            ? Collections.unmodifiableList(new ArrayList<>((Collection<?>) input))
            : input;
        this.changedValues = changedValues == null
            ? Collections.emptyMap()
            : Collections.unmodifiableMap(new LinkedHashMap<>(changedValues));
        this.queryParam = queryParam;
        this.source = source;
        this.resultType = resultType;
        this.columnOmission = columnOmission;
        this.virtualUpdate = virtualUpdate;
    }

    /**
     * Remove virtual input columns from this update's physical SQL and pending value map.
     * The original changed-values snapshot remains available to the interceptor.
     *
     * @param columns names or mapped properties consumed outside the main-table update
     */
    public void omitColumns(String... columns) {
        if (columnOmission == null) {
            throw new UnsupportedOperationException("column omission is only available for update executions");
        }
        columnOmission.accept(columns);
    }

    /**
     * Execute an UPDATE's virtual mutation through the ordinary awaited update lifecycle,
     * replacing physical SQL without creating a dummy main-table write.
     *
     * <p>The mutation supplier runs only after prepare/before listeners succeed. Its result
     * reaches the normal after listeners, whose failures and cancellation propagate to the
     * caller. The caller must compose a transaction covering all mutation work and compute
     * the true authorized affected-asset count; this method adds neither permissions nor a
     * transaction. Original submitted values remain available to lifecycle listeners even
     * after {@link #omitColumns(String...)} consumes virtual properties.</p>
     *
     * <p>Available only on framework-created UPDATE contexts. Select either the interceptor's
     * native {@code proceed} or this virtual continuation; at most one continuation may be
     * subscribed once per execution. Repeated or mixed subscriptions fail before executing
     * the second mutation. Re-subscribing the outer update creates a new execution context.</p>
     *
     * @param mutation deferred virtual persistence returning the actual matched row count
     * @return lifecycle-aware mutation result, retaining subscriber Reactor Context
     * @throws UnsupportedOperationException when virtual update is unavailable
     */
    public Mono<Integer> executeVirtualUpdate(Supplier<Mono<Integer>> mutation) {
        if (operation != Operation.UPDATE || virtualUpdate == null) {
            throw new UnsupportedOperationException("virtual continuation is only available for update executions");
        }
        return virtualUpdate.apply(Objects.requireNonNull(mutation, "mutation"));
    }
}
