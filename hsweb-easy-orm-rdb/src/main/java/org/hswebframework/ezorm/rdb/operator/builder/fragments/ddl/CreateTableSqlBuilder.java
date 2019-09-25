package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.SqlBuilder;

public interface CreateTableSqlBuilder extends SqlBuilder<RDBTableMetadata> {

    String ID_VALUE = "createTableSqlBuilder";
    FeatureId<CreateTableSqlBuilder> ID =FeatureId.of(ID_VALUE);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return "Create Table SQL 构造器";
    }
}
