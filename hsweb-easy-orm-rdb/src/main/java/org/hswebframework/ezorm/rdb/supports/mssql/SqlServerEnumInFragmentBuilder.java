package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

public class SqlServerEnumInFragmentBuilder extends EnumInFragmentBuilder {

    public static SqlServerEnumInFragmentBuilder notIn = new SqlServerEnumInFragmentBuilder(true);
    public static SqlServerEnumInFragmentBuilder in = new SqlServerEnumInFragmentBuilder(false);


    SqlServerEnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    public PrepareSqlFragments bitAnd(String column, long value) {

        return PrepareSqlFragments.of().addSql(column, "&", String.valueOf(value));
    }
}
