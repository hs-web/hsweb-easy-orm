package org.hswebframework.ezorm.rdb.operator.dml.query;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.operator.dml.FunctionColumn;

@Getter
@Setter
public class SelectColumn extends FunctionColumn {

    private String alias;

    public static SelectColumn of(String name) {
        SelectColumn column = new SelectColumn();
        column.setColumn(name);
        return column;
    }
}
