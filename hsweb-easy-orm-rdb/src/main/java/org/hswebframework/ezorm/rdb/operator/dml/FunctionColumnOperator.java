package org.hswebframework.ezorm.rdb.operator.dml;

public class FunctionColumnOperator implements Operator<FunctionColumn> {

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
