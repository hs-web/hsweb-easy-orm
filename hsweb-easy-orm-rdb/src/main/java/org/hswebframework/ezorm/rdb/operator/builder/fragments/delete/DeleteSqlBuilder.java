package org.hswebframework.ezorm.rdb.operator.builder.fragments.delete;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperatorParameter;

public interface DeleteSqlBuilder  extends SqlBuilder<DeleteOperatorParameter> {

    String ID_VALUE = "deleteSqlBuilder";
    FeatureId<DeleteSqlBuilder> ID =FeatureId.of(ID_VALUE);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return "Delete SQL 构造器";
    }

}
