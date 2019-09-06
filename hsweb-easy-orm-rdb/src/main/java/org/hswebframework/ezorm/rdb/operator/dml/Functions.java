package org.hswebframework.ezorm.rdb.operator.dml;

import org.hswebframework.ezorm.rdb.metadata.RDBFeatures;

public interface Functions {


    static FunctionColumnOperator count(String column){
        return new FunctionColumnOperator(RDBFeatures.count.getFunction(),column);
    }

    static FunctionColumnOperator sum(String column){
        return new FunctionColumnOperator(RDBFeatures.sum.getFunction(),column);
    }

}
