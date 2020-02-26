package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;

public interface CreateIndexSqlBuilder extends SqlBuilder<CreateIndexParameter> {

    String ID_VALUE = "createIndexSqlBuilder";

    FeatureId<CreateIndexSqlBuilder> ID =FeatureId.of(ID_VALUE);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return "索引SQL构造器";
    }
}
