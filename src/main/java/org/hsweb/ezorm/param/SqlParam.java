package org.hsweb.ezorm.param;


import java.util.*;
import java.util.stream.Collectors;

/**
 * Created by zhouhao on 16-4-19.
 */
public class SqlParam<R extends SqlParam> implements Cloneable{

    /**
     * 条件
     */
    protected List<Term> terms = new LinkedList<>();

    /**
     * 指定要处理的字段
     */
    protected Set<String> includes = new LinkedHashSet<>();

    /**
     * 指定不处理的字段
     */
    protected Set<String> excludes = new LinkedHashSet<>();

    public R or(String termString, Object value) {
        Term term = new Term();
        term.setField(termString);
        term.setValue(value);
        term.setType(Term.Type.or);
        terms.add(term);
        return (R) this;
    }

    public R and(String termString, Object value) {
        Term term = new Term();
        term.setField(termString);
        term.setValue(value);
        term.setType(Term.Type.and);
        terms.add(term);
        return (R) this;
    }

    public Term nest() {
        return nest(null, null);
    }

    public Term orNest() {
        return orNest(null, null);
    }

    public Term nest(String termString, Object value) {
        Term term = new Term();
        term.setField(termString);
        term.setValue(value);
        term.setType(Term.Type.and);
        terms.add(term);
        return term;
    }

    public Term orNest(String termString, Object value) {
        Term term = new Term();
        term.setField(termString);
        term.setValue(value);
        term.setType(Term.Type.or);
        terms.add(term);
        return term;
    }


    public R includes(String... fields) {
        includes.addAll(Arrays.asList(fields));
        return (R) this;
    }

    public R excludes(String... fields) {
        excludes.addAll(Arrays.asList(fields));
        includes.removeAll(Arrays.asList(fields));
        return (R) this;
    }

    public R where(String key, Object value) {
        and(key, value);
        return (R) this;
    }

    public Set<String> getIncludes() {
        if (includes == null) includes = new LinkedHashSet<>();
        return includes;
    }

    public Set<String> getExcludes() {
        if (excludes == null) excludes = new LinkedHashSet<>();
        return excludes;
    }

    public void setIncludes(Set<String> includes) {
        this.includes = includes;
    }

    public void setExcludes(Set<String> excludes) {
        this.excludes = excludes;
    }

    public List<Term> getTerms() {
        return terms;
    }

    public void setTerms(List<Term> terms) {
        this.terms = terms;
    }

    @Override
    public SqlParam<R> clone()  {
        SqlParam<R> sqlParam=new SqlParam<>();
        sqlParam.setExcludes(new LinkedHashSet<>(excludes));
        sqlParam.setIncludes(new LinkedHashSet<>(includes));
        List<Term> terms = this.terms.stream().map(term -> term.clone()).collect(Collectors.toList());
        sqlParam.setTerms(terms);
        return sqlParam;
    }
}
