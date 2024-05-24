package org.hswebframework.ezorm.rdb.operator.builder.fragments.function;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.DynamicFeatures;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.delete.DeleteSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.FunctionColumn;

import java.util.Map;

public interface FunctionFragmentBuilder extends Feature {

    static FeatureId<FunctionFragmentBuilder> createFeatureId(String suffix) {
       return DynamicFeatures.lookup(RDBFeatureType.function, suffix);
    }

    @Override
    default String getId() {
        return createFeatureId(getFunction()).getId();
    }

    @Override
    default RDBFeatureType getType() {
        return RDBFeatureType.function;
    }

    String getFunction();

    SqlFragments create(String columnFullName, RDBColumnMetadata metadata, Map<String, Object> opts);

    default SqlFragments create(String columnFullName, RDBColumnMetadata metadata, FunctionColumn column) {
        return create(columnFullName, metadata, column.getOpts());
    }
}
