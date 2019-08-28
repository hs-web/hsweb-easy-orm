package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.core.meta.ColumnMetaData;
import org.hswebframework.ezorm.core.meta.ObjectMetaData;
import org.hswebframework.ezorm.core.meta.ObjectType;

public interface ForeignKeyMetadata extends ObjectMetaData {

    @Override
    default ObjectType getObjectType() {
        return RDBObjectType.foreign_key;
    }

    boolean isLogical();

    boolean isToMany();

    DQLObjectMetaData getSource();

    DQLObjectMetaData getTarget();

    ColumnMetaData getSourceColumn();

    ColumnMetaData getTargetColumn();

    Terms getJoinTerms();
}
