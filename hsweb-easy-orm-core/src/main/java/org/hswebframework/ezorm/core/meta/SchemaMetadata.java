package org.hswebframework.ezorm.core.meta;

import java.util.List;
import java.util.Optional;

/**
 * @author zhouhao
 * @since 4.0.0
 */
public interface SchemaMetadata extends ObjectMetadata, FeatureSupportedMetadata {

    String getName();

    DatabaseMetadata getDatabase();

    List<ObjectType> getAllObjectType();

    <T extends ObjectMetadata> List<T> getObject(ObjectType type);

    <T extends ObjectMetadata> Optional<T> getObject(ObjectType type, String name);

    <T extends ObjectMetadata> Optional<T> getObject(ObjectType type, String name, boolean autoLoad);

    <T extends ObjectMetadata> Optional<T> removeObject(ObjectType type, String name);

    void addObject(ObjectMetadata metadata);
}
