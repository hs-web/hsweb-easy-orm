package org.hswebframework.ezorm.rdb.operator.builder.fragments.delete;

import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperatorParameter;

public interface DeleteSqlBuilder  extends SqlBuilder<DeleteOperatorParameter> {

    String id = "deleteSqlBuilder";

    @Override
    default String getId() {
        return id;
    }

    @Override
    default String getName() {
        return "Delete SQL 构造器";
    }

}
