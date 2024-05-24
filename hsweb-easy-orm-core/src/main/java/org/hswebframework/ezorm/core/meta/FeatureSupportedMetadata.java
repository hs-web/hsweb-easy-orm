package org.hswebframework.ezorm.core.meta;

import org.apache.commons.collections.MapUtils;
import org.hswebframework.ezorm.core.CastUtil;
import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.FeatureType;

import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
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

    default <T extends Feature> Optional<T> getFeature(FeatureType type) {
        Map<String, Feature> features = getFeatures();
        if (MapUtils.isEmpty(features)) {
            return Optional.empty();
        }
        for (Feature value : features.values()) {
            if (value.getType().getId().equals(type.getId())) {
                return Optional.of((T) value);
            }
        }
        return Optional.empty();
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

    default <T extends Feature> T getFeatureNow(FeatureId<T> id) {
        return getFeatureNow(id.getId());
    }

    default <T extends Feature> Optional<T> findFeature(FeatureId<T> id) {
        return findFeature(id.getId());
    }

    default <T extends Feature> T findFeatureNow(FeatureId<T> id) {
        return this.findFeatureNow(id.getId());
    }

    default <T extends Feature> Optional<T> findFeature(String id) {
        return Optional.ofNullable(findFeatureOrElse(id, null));
    }

    default <T extends Feature> T findFeatureOrElse(FeatureId<T> id, Supplier<T> orElse) {
        return findFeatureOrElse(id.getId(), orElse);
    }

    default <T extends Feature> T findFeatureOrElse(String id, Supplier<T> orElse) {
        return getFeatureOrElse(id, orElse);
    }

    default <T extends Feature> T findFeatureNow(String id) {
        return this.findFeatureOrElse(id, () -> {
            throw new UnsupportedOperationException("unsupported feature " + id);
        });
    }

    default <T extends Feature> T getFeatureNow(String id) {
        return this.getFeatureOrElse(id, () -> {
            throw new UnsupportedOperationException("unsupported feature " + id);
        });
    }

    default <T extends Feature> T getFeatureOrElse(String id, Supplier<T> orElse) {
        Map<String, Feature> featureMap = getFeatures();
        Feature feature = featureMap.get(id);
        if (feature != null) {
            return CastUtil.cast(feature);
        }
        return orElse == null ? null : orElse.get();
    }

    default <T extends Feature> Optional<T> getFeature(String id) {
        return Optional.ofNullable(getFeatureOrElse(id, null));
    }

    default <T extends Feature> Optional<T> getFeature(Feature target) {
        return getFeature(target.getId());
    }

    default boolean supportFeature(String id) {
        return getFeature(id).isPresent();
    }

    default boolean supportFeature(Feature feature) {
        return getFeature(feature.getId()).isPresent();
    }

}
