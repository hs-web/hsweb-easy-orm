package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

public class MysqlEnumInFragmentBuilder extends EnumInFragmentBuilder {

    public static MysqlEnumInFragmentBuilder notIn = new MysqlEnumInFragmentBuilder(true);
    public static MysqlEnumInFragmentBuilder in = new MysqlEnumInFragmentBuilder(false);


    MysqlEnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    public PrepareSqlFragments bitAnd(String column, long value) {

        return PrepareSqlFragments.of().addSql(column,"&", String.valueOf(value));
    }
}
