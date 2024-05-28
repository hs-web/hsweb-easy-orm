package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

public class SqlServerEnumInFragmentBuilder extends EnumInFragmentBuilder {

    public static SqlServerEnumInFragmentBuilder notIn = new SqlServerEnumInFragmentBuilder(true);
    public static SqlServerEnumInFragmentBuilder in = new SqlServerEnumInFragmentBuilder(false);


    SqlServerEnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    public SqlFragments bitAnd(String column, long value) {
        return SqlFragments.of(column, "&", String.valueOf(value));
    }
}
