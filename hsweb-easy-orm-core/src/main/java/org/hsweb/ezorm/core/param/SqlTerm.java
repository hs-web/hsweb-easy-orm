package org.hsweb.ezorm.core.param;

/**
 * @author zhouhao
 */
public class SqlTerm extends Term {

    @Override
    public Term clone() {
        SqlTerm term = new SqlTerm();
        term.setColumn(getColumn());
        term.setValue(getValue());
        term.setTermType(getTermType());
        term.setType(getType());
        getTerms().forEach(t -> term.addTerm(t.clone()));
        return term;
    }
}
