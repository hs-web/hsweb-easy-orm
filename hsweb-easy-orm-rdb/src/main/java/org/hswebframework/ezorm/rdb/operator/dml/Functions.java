package org.hswebframework.ezorm.rdb.operator.dml;

import org.hswebframework.ezorm.rdb.metadata.RDBFutures;

public interface Functions {


    static FunctionColumnOperator count(String column){
        return new FunctionColumnOperator(RDBFutures.count.getFunction(),column);
    }

    static FunctionColumnOperator sum(String column){
        return new FunctionColumnOperator(RDBFutures.sum.getFunction(),column);
    }

}
