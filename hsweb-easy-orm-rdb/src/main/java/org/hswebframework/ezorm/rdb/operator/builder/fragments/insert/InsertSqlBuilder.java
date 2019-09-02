package org.hswebframework.ezorm.rdb.operator.builder.fragments.insert;

import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;

public interface InsertSqlBuilder extends SqlBuilder<InsertOperatorParameter> {

    String id = "insertSqlBuilder";

    @Override
    default String getId() {
        return id;
    }

    @Override
    default String getName() {
        return "Insert SQL 构造器";
    }
}
