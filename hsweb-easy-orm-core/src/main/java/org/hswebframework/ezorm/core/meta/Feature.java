package org.hswebframework.ezorm.core.meta;

import org.hswebframework.ezorm.core.FeatureType;

public interface Feature {

    String getId();

    FeatureType getType();

    String getText();

}
