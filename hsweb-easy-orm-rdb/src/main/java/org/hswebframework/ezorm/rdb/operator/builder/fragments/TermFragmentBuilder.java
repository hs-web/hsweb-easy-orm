package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;

/**
 * SQL条件片段构造器
 *
 * @author zhouhao
 * @since 4.0.0
 */
public interface TermFragmentBuilder extends Feature {

    @Override
    default String getId() {
        return getType().getFeatureId(getTermType());
    }

    @Override
    default RDBFeatureType getType() {
        return RDBFeatureType.termType;
    }

    String getTermType();

    SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term);

}
