package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.WhereFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EqTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.IsNullTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.NotNullTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.NotTermFragmentBuilder;

public interface RDBFutures {


    String where = "where";


    static WhereFragmentBuilder where(TableOrViewMetadata metadata) {
        return WhereFragmentBuilder.of(metadata);
    }


    EqTermFragmentBuilder eq = new EqTermFragmentBuilder("eq", "等于");

    NotTermFragmentBuilder not = new NotTermFragmentBuilder("not", "不等于");

    IsNullTermFragmentBuilder isNull = new IsNullTermFragmentBuilder("isnull", "为空");

    NotNullTermFragmentBuilder notNull = new NotNullTermFragmentBuilder("notnull", "不为空");


}
