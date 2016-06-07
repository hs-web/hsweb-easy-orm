package org.hsweb.ezorm.param;

import java.util.LinkedList;
import java.util.List;

/**
 * 执行条件
 * Created by zhouhao on 16-5-9.
 */
public class Term implements Cloneable {

    /**
     * 字段
     */
    private String field;

    /**
     * 值
     */
    private Object value;

    /**
     * 链接类型
     */
    private Type type = Type.and;

    /**
     * 条件类型
     */
    private String termType = TermType.eq;

    /**
     * 嵌套的条件
     */
    private List<Term> terms = new LinkedList<>();


    public Term or(String term, Object value) {
        Term queryTerm = new Term();
        queryTerm.setField(term);
        queryTerm.setValue(value);
        queryTerm.setType(Type.or);
        terms.add(queryTerm);
        return this;
    }

    public Term and(String term, Object value) {
        Term queryTerm = new Term();
        queryTerm.setField(term);
        queryTerm.setValue(value);
        queryTerm.setType(Type.and);
        terms.add(queryTerm);
        return this;
    }

    public Term nest() {
        return nest(null, null);
    }

    public Term orNest() {
        return orNest(null, null);
    }

    public Term nest(String term, Object value) {
        Term queryTerm = new Term();
        queryTerm.setField(term);
        queryTerm.setValue(value);
        queryTerm.setType(Type.and);
        terms.add(queryTerm);
        return queryTerm;
    }

    public Term orNest(String term, Object value) {
        Term queryTerm = new Term();
        queryTerm.setField(term);
        queryTerm.setValue(value);
        queryTerm.setType(Type.or);
        terms.add(queryTerm);
        return queryTerm;
    }

    public String getField() {
        return field;
    }

    public void setField(String field) {
        if (field == null) return;
        if (field.contains("$")) {
            String tmp[] = field.split("[\\$]");
            setTermType(tmp[1]);
            field = tmp[0];
        }
        this.field = field;
    }

    public Object getValue() {
        return value;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public String getTermType() {
        return termType.toLowerCase();
    }

    public void setTermType(String termType) {
        this.termType = termType;
    }

    public List<Term> getTerms() {
        return terms;
    }

    public void setTerms(List<Term> terms) {
        this.terms = terms;
    }

    public Term addTerm(Term term) {
        terms.add(term);
        return this;
    }

    @Override
    public Term clone() {
        Term term = new Term();
        term.setField(field);
        term.setValue(value);
        term.setTermType(termType);
        term.setType(type);
        terms.forEach(t -> term.addTerm(t.clone()));
        return term;
    }

    public enum Type {
        or, and;

        public static Type fromString(String str) {
            try {
                return Type.valueOf(str.toLowerCase());
            } catch (Exception e) {
                return and;
            }
        }
    }


}
