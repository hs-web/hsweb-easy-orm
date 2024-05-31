package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

public class MysqlEnumInFragmentBuilder extends EnumInFragmentBuilder {

    public static MysqlEnumInFragmentBuilder notIn = new MysqlEnumInFragmentBuilder(true);
    public static MysqlEnumInFragmentBuilder in = new MysqlEnumInFragmentBuilder(false);


    MysqlEnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    public SqlFragments bitAnd(String column, long value) {

        return SqlFragments.of(column, "&", String.valueOf(value));
    }
}
