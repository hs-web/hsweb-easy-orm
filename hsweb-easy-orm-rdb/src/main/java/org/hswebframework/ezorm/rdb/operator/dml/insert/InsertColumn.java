package org.hswebframework.ezorm.rdb.operator.dml.insert;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.operator.dml.FunctionColumn;

@Getter
@Setter
public class InsertColumn extends FunctionColumn {

    private Object value;
}
