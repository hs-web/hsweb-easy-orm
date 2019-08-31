package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.meta.RDBFeatureType;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import java.util.List;

public interface TermsFragmentBuilder extends Feature {

    @Override
    default String getId() {
        return getType().getId();
    }

    @Override
    default String getName() {
        return "SQL条件组合";
    }

    @Override
    default RDBFeatureType getType() {
        return RDBFeatureType.termsType;
    }

    SqlFragments createFragments(String termPrefix, List<Term> terms, String... tableAlias);

}
