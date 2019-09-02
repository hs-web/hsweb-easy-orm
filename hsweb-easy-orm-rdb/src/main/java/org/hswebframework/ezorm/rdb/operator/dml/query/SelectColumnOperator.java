package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.rdb.operator.dml.Operator;

public class SelectColumnOperator implements Operator<SelectColumn> {
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

    public Operator<SelectColumn> as(String alias) {
        column.setAlias(alias);
        return this;
    }
}
