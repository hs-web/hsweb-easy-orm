package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

class OracleEnumInFragmentBuilder extends EnumInFragmentBuilder {

    static OracleEnumInFragmentBuilder notIn = new OracleEnumInFragmentBuilder(true);
    static OracleEnumInFragmentBuilder in = new OracleEnumInFragmentBuilder(false);


    OracleEnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    protected PrepareSqlFragments bitAnd(String column, long value) {

        return PrepareSqlFragments.of().addSql("BITAND(",column,",", "?)").addParameter(value);
    }
}
