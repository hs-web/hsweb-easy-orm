package org.hswebframework.ezorm.core;

import org.hswebframework.ezorm.core.meta.DefaultFeatureType;
import org.hswebframework.ezorm.core.meta.Feature;

import java.util.UUID;

public interface DefaultValueGenerator extends Feature {

    FeatureId<DefaultValueGenerator> uuid = createId("uuid");

    FeatureId<DefaultValueGenerator> currentTimestamp = createId("curr_time");

    static FeatureId<DefaultValueGenerator> createId(String id) {
        return FeatureId.of("generator_".concat(id));
    }

    String getSortId();

    @Override
    default String getId() {
        return "generator_".concat(getSortId());
    }

    @Override
    default FeatureType getType() {
        return DefaultFeatureType.defaultValueGenerator;
    }

    DefaultValue generate();

}
