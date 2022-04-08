package org.hswebframework.ezorm.rdb.operator.dml.query;


import org.hswebframework.ezorm.rdb.operator.dml.SelectColumnSupplier;

import java.util.Map;
import java.util.function.Supplier;

public class SelectColumnOperator implements SelectColumnSupplier {
    private final SelectColumn column = new SelectColumn();

    public SelectColumnOperator(String name){
        column.setColumn(name);
    }
    public SelectColumnOperator(String name, String function){
        column.setFunction(function);
        column.setColumn(name);
    }

    public SelectColumnOperator(String name, String function, Map<String,Object> opts){
        column.setFunction(function);
        column.setColumn(name);
        column.setOpts(opts);
    }
    @Override
    public SelectColumn get() {
        return column;
    }

    public SelectColumnSupplier as(String alias) {
        column.setAlias(alias);
        return this;
    }
}
