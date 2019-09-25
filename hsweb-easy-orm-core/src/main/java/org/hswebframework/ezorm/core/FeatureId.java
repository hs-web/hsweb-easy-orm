package org.hswebframework.ezorm.core;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.core.meta.Feature;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class FeatureId<T extends Feature> {

    private String id;

    public static <T extends Feature> FeatureId<T> of(String id){
        return new FeatureId<>(id);
    }
}
