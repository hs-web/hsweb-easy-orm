package org.hswebframework.ezorm.rdb.operator.dml.update;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.operator.dml.FunctionColumn;

@Getter
@Setter
public class UpdateColumn extends FunctionColumn {

    private Object value;

}
