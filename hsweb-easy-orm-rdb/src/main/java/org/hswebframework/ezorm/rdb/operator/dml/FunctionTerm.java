package org.hswebframework.ezorm.rdb.operator.dml;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.param.Term;

import java.util.Map;

@Getter
@Setter
public class FunctionTerm extends Term {

    private String function;

    private Map<String, String> opts;

}
