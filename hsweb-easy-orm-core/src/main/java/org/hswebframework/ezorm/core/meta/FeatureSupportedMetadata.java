package org.hswebframework.ezorm.core.meta;

import org.hswebframework.ezorm.core.CastUtil;
import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.FeatureType;

import java.util.*;
import java.util.stream.Collectors;

import static java.util.Optional.*;

public interface FeatureSupportedMetadata {

    Map<String, Feature> getFeatures();

    void addFeature(Feature feature);

    default List<Feature> getFeatureList() {
        return Optional.ofNullable(getFeatures())
                .map(Map::values)
                .<List<Feature>>map(ArrayList::new)
                .orElseGet(Collections::emptyList);
    }

    default <T extends Feature> List<T> getFeatures(FeatureType type) {

        return ofNullable(getFeatures())
                .map(features -> features.values()
                        .stream()
                        .filter(feature -> feature.getType().equals(type))
                        .map(CastUtil::<T>cast)
                        .collect(Collectors.toList()))
                .orElseGet(Collections::emptyList);

    }

    default <T extends Feature> Optional<T> getFeature(FeatureId<T> id) {
        return getFeature(id.getId());
    }

    default <T extends Feature> Optional<T> getFeature(String id) {
        return ofNullable(getFeatures())
                .map(feature -> feature.get(id))
                .map(CastUtil::cast);
    }

    default <T extends Feature> Optional<T> getFeature(Feature target) {
        return ofNullable(getFeatures())
                .map(feature -> feature.get(target.getId()))
                .map(CastUtil::cast);
    }

    default boolean supportFeature(String id) {
        return getFeature(id).isPresent();
    }

    default boolean supportFeature(Feature feature) {
        return getFeature(feature.getId()).isPresent();
    }

}
