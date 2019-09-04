package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.rdb.operator.dml.FunctionColumn;
import org.hswebframework.ezorm.rdb.operator.dml.Functions;
import org.hswebframework.ezorm.rdb.operator.dml.Operator;

public interface Orders {

    static OrderOperator function(Operator<FunctionColumn> column){
        return new OrderOperator(column.get());
    }

    static OrderOperator count(String name){
        return function(Functions.count(name));
    }

    static Operator<SortOrder> asc(String column){
        return new OrderOperator(column).asc();
    }

    static Operator<SortOrder> desc(String column){
        return new OrderOperator(column).desc();
    }
}
