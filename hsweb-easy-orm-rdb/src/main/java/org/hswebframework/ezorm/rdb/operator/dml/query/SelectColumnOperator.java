package org.hswebframework.ezorm.rdb.operator.dml.query;


import java.util.function.Supplier;

public class SelectColumnOperator implements Supplier<SelectColumn> {
    private SelectColumn column = new SelectColumn();

    public SelectColumnOperator(String name){
        column.setColumn(name);
    }
    public SelectColumnOperator(String name, String function){
        column.setFunction(function);
        column.setColumn(name);
    }
    @Override
    public SelectColumn get() {
        return column;
    }

    public Supplier<SelectColumn> as(String alias) {
        column.setAlias(alias);
        return this;
    }
}
