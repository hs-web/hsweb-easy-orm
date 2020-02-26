package org.hswebframework.ezorm.rdb.operator.dml;

import java.util.function.Supplier;

public class FunctionColumnOperator implements Supplier<FunctionColumn> {

    private FunctionColumn column=new FunctionColumn();

    public FunctionColumnOperator(String function,String columnName){
        column.setColumn(columnName);
        column.setFunction(function);
    }


    @Override
    public FunctionColumn get() {

        return column;
    }
}
