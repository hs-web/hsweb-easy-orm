package org.hswebframework.ezorm.core;

import org.hswebframework.ezorm.core.meta.Feature;

public interface FeatureId<T extends Feature> {

    String getId();

    static <T extends Feature> FeatureId<T> of(String id) {
        return () -> id;
    }
}
