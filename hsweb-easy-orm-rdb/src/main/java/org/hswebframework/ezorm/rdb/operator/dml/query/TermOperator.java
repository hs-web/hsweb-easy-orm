package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.dml.Operator;

public class TermOperator implements Operator<Term> {

    private Term term = new Term();

    public TermOperator(String column, String termType, Object value) {
        term.setColumn(column);
        term.setTermType(termType);
        term.setValue(value);
    }

    @Override
    public Term get() {
        return term;
    }
}
