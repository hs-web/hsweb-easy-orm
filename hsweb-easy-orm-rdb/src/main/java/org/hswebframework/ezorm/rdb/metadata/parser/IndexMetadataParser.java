package org.hswebframework.ezorm.rdb.metadata.parser;

import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBObjectType;

import java.util.List;

public interface IndexMetadataParser extends ObjectMetadataParser {

    String id = "indexMetadataParser";

    @Override
    default String getId() {
        return id;
    }

    @Override
    default String getName() {
        return "索引解析器";
    }

    @Override
    default ObjectType getObjectType() {
        return RDBObjectType.index;
    }

    List<RDBIndexMetadata> parseTableIndex(String tableName);
}
