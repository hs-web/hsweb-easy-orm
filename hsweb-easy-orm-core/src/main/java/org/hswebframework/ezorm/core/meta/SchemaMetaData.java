package org.hswebframework.ezorm.core.meta;

import java.util.List;
import java.util.Optional;

/**
 * @author zhouhao
 * @since 4.0.0
 */
public interface SchemaMetaData extends ObjectMetaData  {

    String getName();

    List<ObjectType> getAllObjectType();

    <T extends ObjectMetaData> List<T> getObject(ObjectType type);

    <T extends ObjectMetaData> Optional<T> getObject(ObjectType type, String name);

}
