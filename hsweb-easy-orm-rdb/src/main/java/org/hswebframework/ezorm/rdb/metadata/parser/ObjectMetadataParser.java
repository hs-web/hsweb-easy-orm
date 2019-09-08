package org.hswebframework.ezorm.rdb.metadata.parser;

import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;

public interface ObjectMetadataParser extends Feature {
    @Override
    default FeatureType getType() {
        return RDBFeatureType.metadataParser;
    }

    ObjectType getObjectType();
}
