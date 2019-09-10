package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;

public interface CreateIndexSqlBuilder extends SqlBuilder<CreateIndexParameter> {

    String id = "createIndexSqlBuilder";

    @Override
    default String getId() {
        return id;
    }

    @Override
    default String getName() {
        return "索引SQL构造器";
    }
}
