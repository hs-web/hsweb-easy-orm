package org.hswebframework.ezorm.rdb.operator.dml.upsert;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;

public interface SaveOrUpdateOperator extends Feature {

    String ID_VALUE = "saveOrUpdateOperator";

    FeatureId<SaveOrUpdateOperator> ID = FeatureId.of(ID_VALUE);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return getType().getName();
    }

    @Override
    default FeatureType getType() {
        return RDBFeatureType.saveOrUpdateOperator;
    }

    SaveResultOperator execute(UpsertOperatorParameter parameter);

}
