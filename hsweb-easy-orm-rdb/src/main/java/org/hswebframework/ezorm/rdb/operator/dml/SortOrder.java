package org.hswebframework.ezorm.rdb.operator.dml;

public class SortOrder extends FunctionColumn {

    private Order order;

    public enum Order {
        asc,
        desc
    }
}
