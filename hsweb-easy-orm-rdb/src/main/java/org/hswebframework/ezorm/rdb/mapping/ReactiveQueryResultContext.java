package org.hswebframework.ezorm.rdb.mapping;

import lombok.Getter;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.rdb.events.EventContext;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;

/**
 * One subscribed entity-query execution shared by preparation listeners and result transformers.
 *
 * <p>The query parameter and attributes belong to this subscription. Listeners can prepare
 * asynchronous operation state with the reactive result holder and retain it in attributes
 * without mutating the reusable query or a singleton transformer.</p>
 *
 * @since 4.2.3
 * @see ReactiveQueryResultTransformer
 */
@Getter
public final class ReactiveQueryResultContext {

    private final TableOrViewMetadata tableMetadata;
    private final EntityColumnMapping columnMapping;
    private final String operation;
    private final QueryParam queryParam;
    private final EventContext attributes = EventContext.create();

    public ReactiveQueryResultContext(TableOrViewMetadata tableMetadata,
                                      EntityColumnMapping columnMapping,
                                      String operation,
                                      QueryParam queryParam) {
        this.tableMetadata = tableMetadata;
        this.columnMapping = columnMapping;
        this.operation = operation;
        this.queryParam = queryParam;
    }
}
