package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;

public interface DropIndexSqlBuilder extends SqlBuilder<CreateIndexParameter> {

    String ID_VALUE = "dropIndexSqlBuilder";
    FeatureId<DropIndexSqlBuilder> ID = FeatureId.of(ID_VALUE);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return "删除SQL构造器";
    }
}
