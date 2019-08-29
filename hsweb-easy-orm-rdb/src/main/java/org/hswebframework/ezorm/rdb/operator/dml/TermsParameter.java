package org.hswebframework.ezorm.rdb.operator.dml;

import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Map;

@Getter
@Setter
public class TermsParameter {

    private List<String> excludes;

    private List<String> includes;

    private List<Term> terms;

    private Map<String, Object> opts;

}
