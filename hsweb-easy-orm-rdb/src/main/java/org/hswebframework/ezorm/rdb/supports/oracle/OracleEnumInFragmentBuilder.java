package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

public class OracleEnumInFragmentBuilder extends EnumInFragmentBuilder {

    public static OracleEnumInFragmentBuilder notIn = new OracleEnumInFragmentBuilder(true);
    public static OracleEnumInFragmentBuilder in = new OracleEnumInFragmentBuilder(false);


    OracleEnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    public PrepareSqlFragments bitAnd(String column, long value) {

        return PrepareSqlFragments.of().addSql("BITAND(",column,",", "?)").addParameter(value);
    }
}
