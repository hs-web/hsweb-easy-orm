package org.hswebframework.ezorm.rdb.operator.builder.fragments.update;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperatorParameter;

public interface UpdateSqlBuilder extends SqlBuilder<UpdateOperatorParameter> {

    String ID_VALUE = "updateSqlBuilder";

    FeatureId<UpdateSqlBuilder> ID = FeatureId.of(ID_VALUE);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return "Update SQL 构造器";
    }
}
