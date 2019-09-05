package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import java.util.List;

public interface ForeignKeyTermFragmentBuilder extends Feature {

    @Override
    default String getId() {
        return getType().getId();
    }

    @Override
    default String getName() {
        return getType().getName();
    }

    @Override
    default FeatureType getType() {
        return RDBFeatureType.foreignKeyTerm;
    }

    SqlFragments createFragments(String tableName, ForeignKeyMetadata key, List<Term> terms);


}
