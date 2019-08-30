package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.meta.RDBFeatureType;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;

public interface QuerySqlFragmentBuilder extends Feature {

    @Override
    default FeatureType getType() {
        return RDBFeatureType.fragment;
    }

    SqlFragments createFragments(ComplexQueryParameter parameter);

}
