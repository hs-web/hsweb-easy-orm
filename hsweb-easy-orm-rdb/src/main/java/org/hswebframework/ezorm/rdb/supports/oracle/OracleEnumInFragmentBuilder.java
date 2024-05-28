package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.SimpleSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

import java.util.Arrays;
import java.util.Collections;

public class OracleEnumInFragmentBuilder extends EnumInFragmentBuilder {

    public static OracleEnumInFragmentBuilder notIn = new OracleEnumInFragmentBuilder(true);
    public static OracleEnumInFragmentBuilder in = new OracleEnumInFragmentBuilder(false);


    OracleEnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    public SqlFragments bitAnd(String column, long value) {
        return SimpleSqlFragments.of(
            Arrays.asList("BITAND(",column,",", "?)"),
            Collections.singletonList(value)
        );
    }
}
