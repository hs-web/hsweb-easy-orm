package org.hswebframework.ezorm.rdb.metadata;

import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.core.meta.ObjectType;

public interface FunctionMetadata extends ObjectMetadata {

    @Override
    default ObjectType getObjectType() {
        return RDBObjectType.function;
    }
}
