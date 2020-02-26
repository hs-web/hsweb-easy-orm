package org.hswebframework.ezorm.rdb.operator;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;

public interface ExceptionTranslation extends Feature {

    String ID_VALUE = "exceptionTranslation";

    FeatureId<ExceptionTranslation> ID = FeatureId.of(ID_VALUE);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
   default String getName(){
        return getType().getName();
    }

    @Override
    default FeatureType getType() {
        return RDBFeatureType.exceptionTranslation;
    }

    Throwable translate(Throwable e);
}
