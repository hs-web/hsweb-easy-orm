package org.hswebframework.ezorm.rdb.operator.builder.fragments.function;

import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import java.util.Map;

public interface FunctionFragmentBuilder extends Feature {

    @Override
    default String getId() {
        return getType().getFeatureId(getFunction());
    }

    @Override
    default RDBFeatureType getType() {
        return RDBFeatureType.function;
    }

    String getFunction();

    SqlFragments create(String columnFullName, RDBColumnMetadata metadata, Map<String, Object> opts);

}
