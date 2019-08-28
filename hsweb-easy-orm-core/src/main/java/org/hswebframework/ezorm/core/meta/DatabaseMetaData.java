package org.hswebframework.ezorm.core.meta;

import java.util.List;
import java.util.Optional;


public interface DatabaseMetaData extends ObjectMetaData {

    String getName();

    SchemaMetaData getCurrentSchema();

    List<SchemaMetaData> getSchemas();

    Optional<SchemaMetaData> getSchema(String name);

    @Override
    default ObjectType getObjectType() {
        return DefaultObjectType.database;
    }
}
