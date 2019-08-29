package org.hswebframework.ezorm.rdb.operator.dml;

import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Map;

@Getter
@Setter
public class Term {

    private String column;

    private Object value;

    private Map<String, String> opts;

    private List<Term> nests;

    private String termType;

    private boolean or;

}
