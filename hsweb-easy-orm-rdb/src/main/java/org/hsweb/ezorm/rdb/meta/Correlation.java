package org.hsweb.ezorm.rdb.meta;

import org.hsweb.ezorm.core.PropertyWrapper;
import org.hsweb.ezorm.core.SimplePropertyWrapper;
import org.hsweb.ezorm.core.param.SqlTerm;
import org.hsweb.ezorm.core.param.Term;

import java.util.*;

public class Correlation implements Cloneable, Comparable<Correlation> {

    public Correlation() {
    }

    public Correlation(String target, String alias, String condition) {
        this.targetTable = target;
        this.alias = alias;
        terms = new ArrayList<>();
        SqlTerm term = new SqlTerm();
        term.setColumn(condition);
        term.setValue(condition);
        term.setSql(condition);
        terms.add(term);
    }

    private String targetTable;

    private String alias;

    private boolean one2one = true;

    private List<Term> terms = new LinkedList<>();

    private Integer index = 0;

    private JOIN join = JOIN.LEFT;

    private String comment;

    private Map<String, Object> properties = new HashMap<>();

    public PropertyWrapper getProperty(String name) {
        return new SimplePropertyWrapper(properties.get(name));
    }

    public PropertyWrapper getProperty(String name, Object defaultValue) {
        return new SimplePropertyWrapper(properties.getOrDefault(name, defaultValue));
    }

    public PropertyWrapper removeProperty(String name) {
        return new SimplePropertyWrapper(properties.remove(name));
    }

    public <T> T setProperty(String property, T value) {
        properties.put(property, value);
        return value;
    }

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
        correlation.setIndex(index);
        correlation.setTargetTable(targetTable);
        terms.forEach(term -> correlation.addTerm(term.clone()));
        return correlation;
    }

    @Override
    public int compareTo(Correlation o) {
        return index.compareTo(o.index);
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

    public void setIndex(Integer index) {
        this.index = index;
    }

    public Integer getIndex() {
        return index;
    }

    public String getComment() {
        return comment;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }
}
