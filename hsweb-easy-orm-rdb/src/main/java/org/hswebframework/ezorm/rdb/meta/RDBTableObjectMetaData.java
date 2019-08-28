package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.core.meta.ObjectType;

import java.util.List;

public class RDBTableObjectMetaData extends AbstractDQLObjectMetaData {


    private List<RDBIndexMetaData> indexies;

    private List<RDBKeyMetadata> keys;

    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.table;
    }
}
