package org.hswebframework.ezorm.core.meta;

import org.hswebframework.ezorm.core.FeatureType;

import java.util.List;
import java.util.Optional;

public interface ObjectMetadataParser extends Feature {
    @Override
    default FeatureType getType() {
        return DefaultFeatureType.metadataParser;
    }

    ObjectType getObjectType();

    <T extends ObjectMetadata> Optional<T> parseByName(String name);

    <T extends ObjectMetadata> List<T> parseAll();

}
