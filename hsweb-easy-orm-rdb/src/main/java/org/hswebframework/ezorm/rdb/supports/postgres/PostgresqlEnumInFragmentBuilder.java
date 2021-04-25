package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

class PostgresqlEnumInFragmentBuilder extends EnumInFragmentBuilder {

    static PostgresqlEnumInFragmentBuilder notIn = new PostgresqlEnumInFragmentBuilder(true);
    static PostgresqlEnumInFragmentBuilder in = new PostgresqlEnumInFragmentBuilder(false);


    PostgresqlEnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    protected PrepareSqlFragments bitAnd(String column, long value) {

        return PrepareSqlFragments.of().addSql(column,"&", String.valueOf(value));
    }
}
