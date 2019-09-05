package org.hswebframework.ezorm.rdb.operator.builder.fragments.query;

import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;

public interface QuerySqlFragmentBuilder extends Feature {

    @Override
    default FeatureType getType() {
        return RDBFeatureType.fragment;
    }

    SqlFragments createFragments(QueryOperatorParameter parameter);

}
