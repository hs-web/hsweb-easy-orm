package org.hswebframework.ezorm.rdb.meta;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.meta.ObjectMetaData;
import org.hswebframework.ezorm.core.meta.ObjectType;

import java.util.Set;

@Getter
@Setter
public class RDBKeyMetadata implements ObjectMetaData {

    private String name;

    private boolean primaryKey;

    private String type;

    private Set<String> columns;

    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.key;
    }
}
