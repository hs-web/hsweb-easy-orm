package org.hswebframework.ezorm.rdb.meta;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.meta.ObjectType;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class RDBTableMetadata extends AbstractTableOrViewMetadata {

    private String comment;

    private List<RDBIndexMetadata> indexes = new ArrayList<>();

    private List<RDBKeyMetadata> keys = new ArrayList<>();


    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.table;
    }
}
