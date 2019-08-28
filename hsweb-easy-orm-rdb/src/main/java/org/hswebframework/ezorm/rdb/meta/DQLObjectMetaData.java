package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.core.meta.ColumnMetaData;
import org.hswebframework.ezorm.core.meta.ObjectMetaData;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.core.meta.SchemaMetaData;

import java.util.List;
import java.util.Optional;


public interface DQLObjectMetaData extends ObjectMetaData {

    SchemaMetaData getSchema();

    List<ColumnMetaData> getColumns();

    List<ColumnMetaData> findColumns();

    Optional<ColumnMetaData> getColumn(String name);

    Optional<ColumnMetaData> findColumn(String name);

    List<ForeignKeyMetadata> getForeignKey();

    Optional<ForeignKeyMetadata> getForeignKey(String targetName);

    @Override
    ObjectType getObjectType();
}
