package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.core.meta.ObjectType;

public class RDBViewMetadata extends AbstractTableOrViewMetadata {
    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.view;
    }
}
