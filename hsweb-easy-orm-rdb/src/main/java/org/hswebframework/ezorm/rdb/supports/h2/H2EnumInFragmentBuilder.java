package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.SimpleSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

import java.util.Arrays;
import java.util.Collections;

public class H2EnumInFragmentBuilder extends EnumInFragmentBuilder {

    public static H2EnumInFragmentBuilder notIn = new H2EnumInFragmentBuilder(true);
    public static H2EnumInFragmentBuilder in = new H2EnumInFragmentBuilder(false);


    H2EnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    public SqlFragments bitAnd(String column, long value) {

        return SimpleSqlFragments.of(
            Arrays.asList("BITAND(", column, ",CAST(? AS BIGINT)", ")"),
            Collections.singletonList(value)
        );
    }
}
