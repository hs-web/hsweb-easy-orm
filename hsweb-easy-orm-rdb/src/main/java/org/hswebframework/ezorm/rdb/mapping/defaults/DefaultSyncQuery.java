package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.context.ContextHolder;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.SyncQuery;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.DMLOperator;
import reactor.util.context.Context;

import java.util.List;
import java.util.Optional;

import static org.hswebframework.ezorm.rdb.events.ContextKeys.source;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;
import static org.hswebframework.ezorm.rdb.mapping.events.EventSupportWrapper.eventWrapper;
import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.*;
import static org.hswebframework.ezorm.rdb.operator.dml.query.Selects.count1;

public class DefaultSyncQuery<T> extends DefaultQuery<T, SyncQuery<T>> implements SyncQuery<T> {

    private final Context context;

    public DefaultSyncQuery(TableOrViewMetadata tableMetadata,
                            EntityColumnMapping mapping,
                            DMLOperator operator,
                            ResultWrapper<T, ?> wrapper,
                            Context context) {
        super(tableMetadata, mapping, operator, wrapper);
        this.context = context;
    }

    @Override
    public List<T> fetch() {
        return ContextHolder.doInContext(context,this::fetch0);
    }

    public List<T> fetch0() {
        return operator
            .query(tableMetadata)
            .context(param.getContext())
            .select(getSelectColumn())
            .selectExcludes(param.getExcludes())
            .where(param.getTerms())
            .orderBy(getSortOrder())
            .when(param.isPaging(), query -> query.paging(param.getPageIndex(), param.getPageSize()))
            .accept(queryOperator ->
                        tableMetadata.fireEvent(MappingEventTypes.select_before, eventContext ->
                            eventContext.set(
                                source(DefaultSyncQuery.this),
                                query(queryOperator),
                                dml(operator),
                                executorType("sync"),
                                type("fetch")
                            )))
            .fetch(eventWrapper(tableMetadata, list(wrapper), executorType("sync"), type("fetch")))
            .sync();
    }

    @Override
    public Optional<T> fetchOne() {
        return ContextHolder.doInContext(context,this::fetchOne0);
    }

    public Optional<T> fetchOne0() {
        return operator
            .query(tableMetadata)
            .context(param.getContext())
            .select(getSelectColumn())
            .where(param.getTerms())
            .orderBy(getSortOrder())
            .paging(0, 1)
            .accept(queryOperator ->
                        tableMetadata.fireEvent(
                            MappingEventTypes.select_before,
                            source(DefaultSyncQuery.this),
                            query(queryOperator),
                            dml(operator),
                            executorType("sync"),
                            type("fetchOne")
                        ))
            .fetch(eventWrapper(tableMetadata, optional(single(wrapper)), executorType("sync"), type("fetchOne")))
            .sync();
    }

    @Override
    public int count() {
        return ContextHolder.doInContext(context,this::count0);
    }

    public int count0() {
        return operator
            .query(tableMetadata)
            .context(param.getContext())
            .select(count1().as("_total"))
            .where(param.getTerms())
            .accept(queryOperator ->
                        tableMetadata.fireEvent(
                            MappingEventTypes.select_before,
                            source(DefaultSyncQuery.this),
                            query(queryOperator),
                            dml(operator),
                            executorType("sync"),
                            type("count")
                        ))
            .fetch(optional(single(column("_total", Number.class::cast))))
            .sync()
            .map(Number::intValue)
            .orElse(0);
    }


}
