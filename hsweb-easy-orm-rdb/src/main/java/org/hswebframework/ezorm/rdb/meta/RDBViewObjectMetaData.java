package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.core.meta.ObjectType;

import java.util.List;

public class RDBViewObjectMetaData extends AbstractDQLObjectMetaData {


    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.view;
    }
}
