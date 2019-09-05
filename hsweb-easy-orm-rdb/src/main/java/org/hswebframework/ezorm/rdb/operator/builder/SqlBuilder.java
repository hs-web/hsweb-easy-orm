package org.hswebframework.ezorm.rdb.operator.builder;

import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;

import static org.hswebframework.ezorm.rdb.metadata.RDBFeatureType.sqlBuilder;

public interface SqlBuilder<T> extends Feature {
    @Override
    default RDBFeatureType getType() {
        return sqlBuilder;
    }

    SqlRequest build(T parameter);
}
