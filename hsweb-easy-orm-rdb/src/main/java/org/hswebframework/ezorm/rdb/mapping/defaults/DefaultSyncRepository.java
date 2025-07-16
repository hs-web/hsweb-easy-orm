package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.apache.commons.collections4.CollectionUtils;
import org.hswebframework.ezorm.rdb.context.ContextHolder;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.SyncDelete;
import org.hswebframework.ezorm.rdb.mapping.SyncQuery;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.SyncUpdate;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.util.context.Context;

import java.util.*;
import java.util.function.Supplier;

public class DefaultSyncRepository<E, K> extends DefaultRepository<E> implements SyncRepository<E, K> {

    private final Context context;

    public DefaultSyncRepository(DatabaseOperator operator, String table, Class<E> type, ResultWrapper<E, ?> wrapper) {
        this(operator,
             () -> operator
                 .getMetadata()
                 .getTable(table)
                 .orElseThrow(() -> new UnsupportedOperationException("table [" + table + "] doesn't exist")), type, wrapper);
    }


    public DefaultSyncRepository(DatabaseOperator operator, RDBTableMetadata table, Class<E> type, ResultWrapper<E, ?> wrapper) {
        this(operator, () -> table, type, wrapper);
    }

    public DefaultSyncRepository(DatabaseOperator operator, Supplier<RDBTableMetadata> table, Class<E> type, ResultWrapper<E, ?> wrapper) {
        super(operator, table, wrapper);
        context = Context.of(Logger.class, createLogger(type));
        initMapping(type);
    }

    protected Logger createLogger(Class<E> type) {
        return LoggerFactory.getLogger(type);
    }

    @Override
    public E newInstance() {
        return wrapper.newRowInstance();
    }

    @Override
    public int deleteById(Collection<K> idList) {
        if (idList == null || idList.isEmpty()) {
            return 0;
        }
        return ContextHolder
            .doInContext(context, () -> createDelete()
                .where().in(getIdColumn(), idList)
                .execute());
    }

    @Override
    public int updateById(K id, E data) {
        if (id == null || data == null) {
            return 0;
        }
        return ContextHolder
            .doInContext(context, () -> createUpdate()
                .set(data)
                .where(getIdColumn(), id)
                .execute());
    }

    @Override
    public SaveResult save(Collection<E> list) {
        return ContextHolder
            .doInContext(context, () -> doSave(list).sync());
    }

    @Override
    public Optional<E> findById(K primaryKey) {
        if (primaryKey == null) {
            return Optional.empty();
        }
        return ContextHolder
            .doInContext(context, () -> createQuery().where(getIdColumn(), primaryKey).fetchOne());
    }

    @Override
    public List<E> findById(Collection<K> primaryKey) {
        if (CollectionUtils.isEmpty(primaryKey)) {
            return new ArrayList<>();
        }
        return ContextHolder
            .doInContext(context, () -> createQuery()
                .where()
                .in(getIdColumn(), primaryKey)
                .fetch());
    }

    @Override
    public void insert(E data) {
        ContextHolder.doInContext(context, () -> doInsert(data).sync());
    }

    @Override
    public int insertBatch(Collection<E> batch) {
        if (CollectionUtils.isEmpty(batch)) {
            return 0;
        }
        return ContextHolder.doInContext(context, () -> doInsert(batch).sync());
    }

    @Override
    public SyncQuery<E> createQuery() {
        return new DefaultSyncQuery<>(getTable(), mapping, operator.dml(), wrapper,context);
    }

    @Override
    public SyncUpdate<E> createUpdate() {
        return new DefaultSyncUpdate<>(getTable(), operator.dml().update(getTable()), mapping,context);
    }

    @Override
    public SyncDelete createDelete() {
        return new DefaultSyncDelete(getTable(), operator.dml().delete(getTable()),context);
    }

    @Override
    public QueryOperator nativeQuery() {
        return operator
            .dml()
            .query(getTable());
    }
}
