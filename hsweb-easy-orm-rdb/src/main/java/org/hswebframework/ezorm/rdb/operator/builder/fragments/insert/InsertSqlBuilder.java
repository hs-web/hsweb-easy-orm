package org.hswebframework.ezorm.rdb.operator.builder.fragments.insert;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;

public interface InsertSqlBuilder extends SqlBuilder<InsertOperatorParameter> {

    String ID_VALUE = "insertSqlBuilder";

    FeatureId<InsertSqlBuilder> ID = FeatureId.of(ID_VALUE);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return "Insert SQL 构造器";
    }
}
