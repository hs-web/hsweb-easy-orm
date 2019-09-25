package org.hswebframework.ezorm.rdb.metadata;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.core.meta.Feature;

public interface ValueCodecFactory extends Feature {

    String ID_VALUE = "valueCodecFactory";

    FeatureId<ValueCodecFactory> ID = FeatureId.of(ID_VALUE);

    @Override
    default FeatureType getType() {
        return RDBFeatureType.codec;
    }

    @Override
    default String getName() {
        return "值编解码器";
    }

    @Override
    default String getId() {
        return ID_VALUE;
    }

    ValueCodec createValueCodec(RDBColumnMetadata column);

}
