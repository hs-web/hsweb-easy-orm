package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import lombok.NoArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;

public class CommonDropIndexSqlBuilder implements DropIndexSqlBuilder {

    public static final CommonDropIndexSqlBuilder INSTANCE=new CommonDropIndexSqlBuilder();

    @Override
    public SqlRequest build(CreateIndexParameter parameter) {
       return PrepareSqlFragments.of("drop index")
                .addSql(parameter.getIndex().getName(), "on", parameter.getTable().getFullName())
                .toRequest();
    }
}
