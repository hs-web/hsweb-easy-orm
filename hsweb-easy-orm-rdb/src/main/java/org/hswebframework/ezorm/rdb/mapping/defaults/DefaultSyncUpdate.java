package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.context.ContextHolder;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.SyncUpdate;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;
import reactor.util.context.Context;

public class DefaultSyncUpdate<E> extends DefaultUpdate<E, SyncUpdate<E>> implements SyncUpdate<E> {

    private final Context context;

    public DefaultSyncUpdate(RDBTableMetadata table, UpdateOperator operator, EntityColumnMapping mapping, Context context) {
        super(table, operator, mapping);
        this.context = context;
    }

    @Override
    public int execute() {
        return ContextHolder.doInContext(context, () -> doExecute().sync());
    }
}
