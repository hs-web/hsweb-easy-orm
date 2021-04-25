package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

class SqlServerEnumInFragmentBuilder extends EnumInFragmentBuilder {

    static SqlServerEnumInFragmentBuilder notIn = new SqlServerEnumInFragmentBuilder(true);
    static SqlServerEnumInFragmentBuilder in = new SqlServerEnumInFragmentBuilder(false);


    SqlServerEnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    protected PrepareSqlFragments bitAnd(String column, long value) {

        return PrepareSqlFragments.of().addSql("bitand(",column,",", "?)").addParameter(value);
    }
}
