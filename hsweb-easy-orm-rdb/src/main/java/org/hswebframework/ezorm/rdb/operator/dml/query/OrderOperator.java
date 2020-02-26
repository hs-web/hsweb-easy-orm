package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.rdb.operator.dml.FunctionColumn;
import org.hswebframework.ezorm.rdb.operator.dml.FunctionTerm;
import org.hswebframework.ezorm.rdb.operator.dml.Operator;
import org.hswebframework.ezorm.rdb.operator.dml.SortOrderSupplier;

import java.util.function.Supplier;

public class OrderOperator implements SortOrderSupplier {

    private SortOrder order = new SortOrder();

    public OrderOperator(String column){
        order.setColumn(column);
    }

    public OrderOperator(FunctionColumn functionColumn){
        order.setColumn(functionColumn.getColumn());
        order.setFunction(functionColumn.getFunction());
        order.setOpts(functionColumn.getOpts());
    }

    public SortOrderSupplier asc() {
        order.setOrder(SortOrder.Order.asc);
        return this;
    }

    public SortOrderSupplier desc() {
        order.setOrder(SortOrder.Order.desc);
        return this;
    }

    @Override
    public SortOrder get() {
        return order;
    }
}
