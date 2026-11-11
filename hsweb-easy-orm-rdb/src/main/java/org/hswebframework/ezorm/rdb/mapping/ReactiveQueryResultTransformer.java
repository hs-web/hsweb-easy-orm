package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.meta.Feature;
import reactor.core.publisher.Flux;

/**
 * Optional asynchronous transformation of mapped reactive entity-query results.
 *
 * <p>Register a feature on entity table metadata. It is invoked after
 * asynchronous select-before preparation and before rows reach the caller. Implementations
 * must preserve row cardinality and order, use bounded buffering, propagate cancellation and
 * failures, and compose rather than subscribe to the source. Count queries never invoke it.</p>
 * <p>Registering this feature also isolates query preparation per subscription, including
 * counts. Tables without it retain their existing select-before event timing.</p>
 *
 * @since 4.2.3
 * @see ReactiveQueryResultContext
 */
public interface ReactiveQueryResultTransformer extends Feature {

    String ID_VALUE = "reactiveQueryResultTransformer";
    FeatureId<ReactiveQueryResultTransformer> ID = FeatureId.of(ID_VALUE);

    /**
     * 声明转换后的完整实体能否进入仅按实体身份索引的共享缓存。
     * 默认不允许；仅当结果不依赖订阅者上下文，且依赖数据更新能由既有缓存失效机制覆盖时返回 true。
     * 缓存命中会跳过查询准备与转换；仓储装饰器仍负责自身权限或其他上下文约束。
     *
     * @return 是否允许共享缓存转换后的结果
     * @since 4.2.3
     */
    default boolean isResultCacheable() {
        return false;
    }

    /**
     * Opts into bounded SQL result consumption before each transformation. A positive size makes
     * the query owner finish each SQL Result before invoking {@link #transform}, allowing enrichment
     * on the same transaction connection without waiting on an unconsumed outer Result. Original
     * order and paging are retained, with primary keys appended for stable segment boundaries.
     * Preparation runs once; transformations share its context across segments. Visibility between
     * SQL segments follows the caller's transaction isolation. Zero retains the single SQL path.
     *
     * @param context prepared subscription-local query metadata
     * @return maximum raw rows per SQL segment, or zero to use the original streaming source
     * @since 4.2.3
     */
    default int getBatchSize(ReactiveQueryResultContext context) {
        return 0;
    }

    /**
     * Transform one mapped entity result stream, retaining the subscriber Reactor context.
     * The context attributes are shared with this execution's select-before listeners only.
     *
     * @param results mapped rows after the database predicate, order, and paging
     * @param context subscription-local execution metadata and prepared attributes
     * @param <T> entity type
     * @return the same logical rows in the same order, with any asynchronous enrichment applied
     */
    <T> Flux<T> transform(Flux<T> results, ReactiveQueryResultContext context);

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
        return MappingFeatureType.reactiveQueryResultTransformer;
    }
}
