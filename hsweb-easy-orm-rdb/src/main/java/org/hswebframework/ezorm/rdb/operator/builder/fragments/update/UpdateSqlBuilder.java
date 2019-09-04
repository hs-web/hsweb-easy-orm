package org.hswebframework.ezorm.rdb.operator.builder.fragments.update;

import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperatorParameter;

public interface UpdateSqlBuilder extends SqlBuilder<UpdateOperatorParameter> {

    String id = "updateSqlBuilder";

    @Override
    default String getId() {
        return id;
    }

    @Override
    default String getName() {
        return "Update SQL 构造器";
    }
}
