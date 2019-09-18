package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.mapping.SyncUpdate;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;

public class DefaultSyncUpdate<E> extends DefaultUpdate<E, SyncUpdate<E>> implements SyncUpdate<E> {

    public DefaultSyncUpdate(RDBTableMetadata table, UpdateOperator operator, Class<E> entityType) {
        super(table, operator, entityType);
    }

    @Override
    public int execute() {
        return doExecute().sync();
    }
}
