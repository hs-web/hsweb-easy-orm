package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;

public interface CreateTableSqlBuilder extends SqlBuilder<RDBTableMetadata> {

    String id = "createTableSqlBuilder";

    @Override
    default String getId() {
        return id;
    }

    @Override
    default String getName() {
        return "Create Table SQL 构造器";
    }
}
