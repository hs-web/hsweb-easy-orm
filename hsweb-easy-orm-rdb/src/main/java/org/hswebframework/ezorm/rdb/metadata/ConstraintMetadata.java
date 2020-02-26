package org.hswebframework.ezorm.rdb.metadata;

import org.hswebframework.ezorm.core.meta.ObjectMetadata;

import java.util.Set;

public interface ConstraintMetadata extends ObjectMetadata {

    @Override
    default RDBObjectType getObjectType() {
        return RDBObjectType.constraint;
    }

    Set<String> getColumns();

    boolean isPrimaryKey();

}
