package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;

public interface DropIndexSqlBuilder extends SqlBuilder<CreateIndexParameter> {

    String id = "dropIndexSqlBuilder";

    @Override
    default String getId() {
        return id;
    }

    @Override
    default String getName() {
        return "删除SQL构造器";
    }
}
