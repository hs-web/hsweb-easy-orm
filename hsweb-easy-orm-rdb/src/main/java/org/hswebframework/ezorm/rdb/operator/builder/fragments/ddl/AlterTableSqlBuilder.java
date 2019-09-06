package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;

public interface AlterTableSqlBuilder extends SqlBuilder<AlterRequest> {

    String id = "alterTableSqlBuilder";

    @Override
    default String getId() {
        return id;
    }

    @Override
    default String getName() {
        return "表结构更新SQL构造器";
    }
}
