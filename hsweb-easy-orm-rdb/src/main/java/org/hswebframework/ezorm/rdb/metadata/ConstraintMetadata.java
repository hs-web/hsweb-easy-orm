package org.hswebframework.ezorm.rdb.metadata;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;

import java.util.Set;

@Getter
@Setter
public class ConstraintMetadata implements ObjectMetadata {

    private String name;

    private String alias;

    private String tableName;

    private Set<String> columns;

    private ConstraintType type;

    @Override
    public RDBObjectType getObjectType() {
        return RDBObjectType.constraint;
    }

    @Override
    @SneakyThrows
    public ObjectMetadata clone() {
        return (ConstraintMetadata) super.clone();
    }

}
