package org.hswebframework.ezorm.rdb.operator.builder;

import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.meta.RDBFeatureType;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

public interface Paginator extends Feature {

    @Override
    default String getId() {
        return getType().getId();
    }

    @Override
    default String getName() {
        return getType().getName();
    }

    @Override
    default RDBFeatureType getType() {
        return RDBFeatureType.paginator;
    }

    SqlFragments doPaging(SqlFragments fragments, int pageIndex, int pageSize);

}
