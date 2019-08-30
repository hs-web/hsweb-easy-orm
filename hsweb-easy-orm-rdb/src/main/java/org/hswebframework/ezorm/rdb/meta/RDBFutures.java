package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.SelectColumnFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.WhereFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.SimpleFunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EqTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.IsNullTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.NotNullTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.NotTermFragmentBuilder;

public interface RDBFutures {


    String where = "where";

    String select = "selectColumns";


    static WhereFragmentBuilder where(TableOrViewMetadata metadata) {
        return WhereFragmentBuilder.of(metadata);
    }

    static SelectColumnFragmentBuilder select(TableOrViewMetadata metadata) {
        return SelectColumnFragmentBuilder.of(metadata);
    }


    /*查询条件*/
    EqTermFragmentBuilder eq = new EqTermFragmentBuilder("eq", "等于");

    NotTermFragmentBuilder not = new NotTermFragmentBuilder("not", "不等于");

    IsNullTermFragmentBuilder isNull = new IsNullTermFragmentBuilder("isnull", "为空");

    NotNullTermFragmentBuilder notNull = new NotNullTermFragmentBuilder("notnull", "不为空");



    /*===============函数==============*/

    /*==聚合函数==*/
    SimpleFunctionFragmentBuilder count = new SimpleFunctionFragmentBuilder("count", "计数");
    SimpleFunctionFragmentBuilder sum = new SimpleFunctionFragmentBuilder("sum", "求和");
    SimpleFunctionFragmentBuilder avg = new SimpleFunctionFragmentBuilder("avg", "平均值");
    SimpleFunctionFragmentBuilder max = new SimpleFunctionFragmentBuilder("max", "最大值");
    SimpleFunctionFragmentBuilder min = new SimpleFunctionFragmentBuilder("min", "最小值");

}
