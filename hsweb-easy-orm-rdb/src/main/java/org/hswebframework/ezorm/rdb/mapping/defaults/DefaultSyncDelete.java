package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.context.ContextHolder;
import org.hswebframework.ezorm.rdb.mapping.SyncDelete;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;
import reactor.util.context.Context;

public class DefaultSyncDelete extends DefaultDelete<SyncDelete> implements SyncDelete {

    private final Context context;

    public DefaultSyncDelete(RDBTableMetadata table, DeleteOperator operator, Context context) {
        super(table, operator);
        this.context = context;
    }

    @Override
    public int execute() {
        return ContextHolder.doInContext(context, () -> doExecute().sync());
    }
}
