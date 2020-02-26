package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.SyncUpdate;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;

public class DefaultSyncUpdate<E> extends DefaultUpdate<E, SyncUpdate<E>> implements SyncUpdate<E> {

    public DefaultSyncUpdate(RDBTableMetadata table, UpdateOperator operator, EntityColumnMapping mapping) {
        super(table, operator, mapping);
    }

    @Override
    public int execute() {
        return doExecute().sync();
    }
}
