package org.hswebframework.ezorm.rdb.metadata;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.core.meta.ObjectType;

import java.util.Set;

@Getter
@Setter
public class RDBKeyMetadata implements ObjectMetadata {

    private String name;

    private String alias;

    private boolean primaryKey;

    private String type;

    private Set<String> columns;

    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.key;
    }
}
