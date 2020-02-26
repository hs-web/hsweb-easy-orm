package org.hswebframework.ezorm.core.meta;

import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;


public interface DatabaseMetadata<S extends SchemaMetadata> extends ObjectMetadata,FeatureSupportedMetadata {

    String getName();

    S getCurrentSchema();

    List<S> getSchemas();

    Optional<S> getSchema(String name);

    <T extends ObjectMetadata> Optional<T> getObject(String name, BiFunction<S, String,  Optional<T>> mapper);

    @Override
    default ObjectType getObjectType() {
        return DefaultObjectType.database;
    }
}
