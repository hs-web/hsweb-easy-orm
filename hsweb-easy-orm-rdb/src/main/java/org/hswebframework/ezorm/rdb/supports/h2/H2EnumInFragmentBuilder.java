package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

public class H2EnumInFragmentBuilder extends EnumInFragmentBuilder {

    public static H2EnumInFragmentBuilder notIn = new H2EnumInFragmentBuilder(true);
    public static H2EnumInFragmentBuilder in = new H2EnumInFragmentBuilder(false);


    H2EnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    public PrepareSqlFragments bitAnd(String column, long value) {

        return PrepareSqlFragments.of().addSql("BITAND(", column, ",CAST(? AS BIGINT)",")").addParameter(value);
    }
}
