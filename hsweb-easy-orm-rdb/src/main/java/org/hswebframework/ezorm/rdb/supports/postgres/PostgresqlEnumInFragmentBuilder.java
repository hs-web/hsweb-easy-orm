package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

public class PostgresqlEnumInFragmentBuilder extends EnumInFragmentBuilder {

    public static PostgresqlEnumInFragmentBuilder notIn = new PostgresqlEnumInFragmentBuilder(true);
    public static PostgresqlEnumInFragmentBuilder in = new PostgresqlEnumInFragmentBuilder(false);


    PostgresqlEnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    public PrepareSqlFragments bitAnd(String column, long value) {

        return PrepareSqlFragments.of().addSql(column,"&", String.valueOf(value));
    }
}
