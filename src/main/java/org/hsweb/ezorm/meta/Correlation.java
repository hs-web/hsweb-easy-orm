package org.hsweb.ezorm.meta;

import org.hsweb.ezorm.param.Term;
import org.hsweb.ezorm.param.TermType;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * Created by zhouhao on 16-6-4.
 */
public class Correlation implements Cloneable {

    public Correlation() {
    }

    public Correlation(String target, String alias, String condition) {
        this.targetTable = target;
        this.alias = alias;
        terms = new ArrayList<>();
        Term term = new Term();
        term.setTermType(TermType.func);
        term.setField(condition);
        term.setValue(condition);
        terms.add(term);
    }

    private String targetTable;

    private String alias;

    private boolean one2one = true;

    private List<Term> terms = new LinkedList<>();

    private JOIN join = JOIN.LEFT;

    public String getTargetTable() {
        return targetTable;
    }

    public void setTargetTable(String targetTable) {
        this.targetTable = targetTable;
    }

    public String getAlias() {
        if (alias == null) alias = targetTable;
        return alias;
    }

    public void setAlias(String alias) {
        this.alias = alias;
    }

    public boolean isOne2one() {
        return one2one;
    }

    public void setOne2one(boolean one2one) {
        this.one2one = one2one;
    }

    public List<Term> getTerms() {
        return terms;
    }

    public void setTerms(List<Term> terms) {
        this.terms = terms;
    }

    public JOIN getJoin() {
        return join;
    }

    public void setJoin(JOIN join) {
        this.join = join;
    }

    public Correlation leftJoin() {
        this.join = JOIN.LEFT;
        return this;
    }

    public Correlation rightJoin() {
        this.join = JOIN.RIGHT;
        return this;
    }

    public Correlation InnerJoin() {
        this.join = JOIN.INNER;
        return this;
    }

    public Correlation FullJoin() {
        this.join = JOIN.FULL;
        return this;
    }

    public Correlation addTerm(Term term) {
        terms.add(term);
        return this;
    }

    @Override
    protected Correlation clone() {
        Correlation correlation = new Correlation();
        correlation.setAlias(alias);
        correlation.setJoin(join);
        correlation.setOne2one(one2one);
        correlation.setTargetTable(targetTable);
        terms.forEach(term -> correlation.addTerm(term.clone()));
        return correlation;
    }

    public enum JOIN {
        LEFT {
            @Override
            public String toString() {
                return "LEFT JOIN";
            }
        }, RIGHT {
            @Override
            public String toString() {
                return "RIGHT JOIN";
            }
        }, FULL {
            @Override
            public String toString() {
                return "FULL JOIN";
            }
        }, INNER {
            @Override
            public String toString() {
                return "JOIN";
            }
        }
    }

}
