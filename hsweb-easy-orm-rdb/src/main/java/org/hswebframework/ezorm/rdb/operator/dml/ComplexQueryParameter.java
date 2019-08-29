package org.hswebframework.ezorm.rdb.operator.dml;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.param.Term;

import java.util.List;

@Getter
@Setter
public class ComplexQueryParameter {

    private List<SelectColumn> select;

    private String from;

    private List<Term> where;

    private List<SortOrder> orderBy;

    private List<FunctionColumn> groupBy;

    private List<FunctionTerm> having;

    private int limit;

    private int offset;

    private boolean forUpdate;


}
