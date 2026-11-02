package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

public interface JsonDataType extends DataType {

    static boolean isJson(RDBColumnMetadata column) {
        if (column == null || column.getType() == null) {
            return false;
        }
        DataType type = column.getType();
        String id = type.getId();
        return type instanceof JsonDataType ||
            "json".equalsIgnoreCase(id) ||
            "jsonb".equalsIgnoreCase(id);
    }
}
