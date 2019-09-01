package org.hswebframework.ezorm.rdb.operator.dml;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.param.Term;

@Getter
@Setter
public class FunctionTerm extends Term {

    private String function;


}
