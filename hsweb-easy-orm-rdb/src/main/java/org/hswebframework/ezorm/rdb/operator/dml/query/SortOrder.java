package org.hswebframework.ezorm.rdb.operator.dml.query;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.operator.dml.FunctionColumn;

@Getter
@Setter
public class SortOrder extends FunctionColumn {

    private Order order = SortOrder.Order.asc;

    public static SortOrder desc(String column) {
        SortOrder order = new SortOrder();
        order.setColumn(column);
        order.setOrder(Order.desc);

        return order;
    }

    public static SortOrder asc(String column) {
        SortOrder order = new SortOrder();
        order.setColumn(column);
        order.setOrder(Order.asc);

        return order;
    }

    public enum Order {
        asc,
        desc
    }
}
