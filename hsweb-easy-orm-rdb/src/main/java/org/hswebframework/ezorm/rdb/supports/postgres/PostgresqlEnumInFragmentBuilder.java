package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

public class PostgresqlEnumInFragmentBuilder extends EnumInFragmentBuilder {

    public static PostgresqlEnumInFragmentBuilder notIn = new PostgresqlEnumInFragmentBuilder(true);
    public static PostgresqlEnumInFragmentBuilder in = new PostgresqlEnumInFragmentBuilder(false);


    PostgresqlEnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    public SqlFragments bitAnd(String column, long value) {
        return SqlFragments.of(column, "&",String.valueOf(value));
    }
}
