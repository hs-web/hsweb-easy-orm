package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexParameter;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.DropIndexSqlBuilder;

public class PostgresqlDropIndexSqlBuilder implements DropIndexSqlBuilder {

    public static final PostgresqlDropIndexSqlBuilder INSTANCE=new PostgresqlDropIndexSqlBuilder();

    @Override
    public SqlRequest build(CreateIndexParameter parameter) {
       return PrepareSqlFragments.of("drop index")
                .addSql(parameter.getIndex().getName())
                .toRequest();
    }
}
