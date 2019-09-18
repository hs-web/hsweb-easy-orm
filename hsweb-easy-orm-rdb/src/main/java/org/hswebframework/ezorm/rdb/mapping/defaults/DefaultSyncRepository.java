package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.SyncDelete;
import org.hswebframework.ezorm.rdb.mapping.SyncQuery;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.SyncUpdate;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;

import java.util.Collection;
import java.util.Optional;

public class DefaultSyncRepository<E, PK> extends DefaultRepository<E> implements SyncRepository<E, PK> {
    public DefaultSyncRepository(DatabaseOperator operator, RDBTableMetadata table, Class<E> type, ResultWrapper<E, ?> wrapper) {
        super(operator, table, type, wrapper);
    }

    @Override
    public Optional<E> findById(PK primaryKey) {
        return Optional.ofNullable(primaryKey)
                .flatMap(pk -> createQuery().where(idColumn, pk).fetchOne());
    }

    @Override
    public void insert(E data) {
        doInsert(data).sync();
    }

    @Override
    public int insertBatch(Collection<E> batch) {
        return doInsert(batch).sync();
    }

    @Override
    public SyncQuery<E> createQuery() {
        return new DefaultSyncQuery<>(table, entityType, operator.dml(), wrapper);
    }

    @Override
    public SyncUpdate<E> createUpdate() {
        return new DefaultSyncUpdate<>(table, operator.dml().update(table.getFullName()), entityType);
    }

    @Override
    public SyncDelete createDelete() {
        return new DefaultSyncDelete(operator.dml().delete(table.getFullName()));
    }
}
