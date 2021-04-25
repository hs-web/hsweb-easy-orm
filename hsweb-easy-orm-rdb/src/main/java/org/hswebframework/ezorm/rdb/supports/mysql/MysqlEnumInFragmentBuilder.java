package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;

class MysqlEnumInFragmentBuilder extends EnumInFragmentBuilder {

    static MysqlEnumInFragmentBuilder notIn = new MysqlEnumInFragmentBuilder(true);
    static MysqlEnumInFragmentBuilder in = new MysqlEnumInFragmentBuilder(false);


    MysqlEnumInFragmentBuilder(boolean not) {
        super(not);
    }

    @Override
    protected PrepareSqlFragments bitAnd(String column, long value) {

        return PrepareSqlFragments.of().addSql(column,"&", String.valueOf(value));
    }
}
