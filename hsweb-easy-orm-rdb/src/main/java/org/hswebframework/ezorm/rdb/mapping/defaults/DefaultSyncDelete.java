package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.mapping.DSLDelete;
import org.hswebframework.ezorm.rdb.mapping.SyncDelete;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;

public class DefaultSyncDelete extends DefaultDelete<SyncDelete> implements SyncDelete {
    public DefaultSyncDelete(DeleteOperator operator) {
        super(operator);
    }

    @Override
    public int execute() {
        return doExecute().sync();
    }
}
