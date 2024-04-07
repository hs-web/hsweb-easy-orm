package org.hswebframework.ezorm.rdb.operator.dml.update;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.operator.dml.FunctionColumn;

@Getter
@Setter
@EqualsAndHashCode(callSuper = true,exclude = "value")
public class UpdateColumn extends FunctionColumn {

    private Object value;

}
