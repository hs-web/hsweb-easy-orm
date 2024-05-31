package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;

import java.util.HashMap;

public class DynamicFeatures {

    private static final Cache[] caches;

    static {
        RDBFeatureType[] all = RDBFeatureType.values();
        caches = new Cache[all.length];
        for (int i = 0; i < all.length; i++) {
            caches[i] = new Cache(all[i]);
        }
    }

    @SuppressWarnings("all")
    public static <T extends Feature> FeatureId<T> lookup(
        RDBFeatureType featureType,
        String id) {

        return (FeatureId) caches[featureType.ordinal()].getOrLoad(id);
    }

    @AllArgsConstructor
    static class Cache extends HashMap<String, FeatureId<?>> {
        private final RDBFeatureType type;

        public FeatureId<?> getOrLoad(String id) {
            FeatureId<?> val = super.get(id);
            if (val == null) {
                synchronized (this) {
                    val = FeatureId.of(type.getFeatureId(id.toLowerCase()));
                    put(id, val);
                }
            }
            return val;
        }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    static class CacheKey {
        private RDBFeatureType type;
        private String id;
    }
}
