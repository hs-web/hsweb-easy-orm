package org.hswebframework.ezorm.rdb.operator.dml.query;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.operator.dml.FunctionColumn;

@Getter
@Setter
public class SortOrder extends FunctionColumn {

    private Order order = SortOrder.Order.asc;

    public enum Order {
        asc,
        desc
    }
}
