package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;

public interface AlterTableSqlBuilder extends SqlBuilder<AlterRequest> {

    String ID_VALUE = "alterTableSqlBuilder";

    FeatureId<AlterTableSqlBuilder> ID =FeatureId.of(ID_VALUE);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return "表结构更新SQL构造器";
    }
}
