package org.hswebframework.ezorm.core;

import org.hswebframework.ezorm.core.meta.DefaultFeatureType;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.core.utils.StringUtils;

import java.util.UUID;

public interface DefaultValueGenerator<E extends ObjectMetadata> extends Feature {

    static <E extends ObjectMetadata> FeatureId<DefaultValueGenerator<E>> createId(String id) {
        return FeatureId.of(StringUtils.concat("generator_",id));
    }

    String getSortId();

    @Override
    default String getId() {
        return StringUtils.concat("generator_",getSortId());
    }

    @Override
    default FeatureType getType() {
        return DefaultFeatureType.defaultValueGenerator;
    }

    DefaultValue generate(E metadata);

}
