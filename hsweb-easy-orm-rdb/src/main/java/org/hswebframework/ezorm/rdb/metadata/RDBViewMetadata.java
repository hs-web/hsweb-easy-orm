package org.hswebframework.ezorm.rdb.metadata;

import org.hswebframework.ezorm.core.meta.ObjectType;

public class RDBViewMetadata extends AbstractTableOrViewMetadata {
    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.view;
    }
}
