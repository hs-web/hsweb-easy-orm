package org.hswebframework.ezorm.core.param;


import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.SneakyThrows;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

/**
 * SQL参数对象
 *
 * @author zhouhao
 * @since 1.0
 */
@SuppressWarnings("all")
@Getter
@Setter
public class Param implements Cloneable, Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 条件
     */
    @NonNull
    protected List<Term> terms = new LinkedList<>();

    /**
     * 指定要处理的字段
     */
    @NonNull
    protected Set<String> includes = new LinkedHashSet<>();

    /**
     * 指定不处理的字段
     */
    @NonNull
    protected Set<String> excludes = new LinkedHashSet<>();

    public <T extends Param> T or(String column, String termType, Object value) {
        Term term = new Term();
        term.setTermType(termType);
        term.setColumn(column);
        term.setValue(value);
        term.setType(Term.Type.or);
        terms.add(term);
        return (T) this;
    }

    public <T extends Param> T and(String column, String termType, Object value) {
        Term term = new Term();
        term.setTermType(termType);
        term.setColumn(column);
        term.setValue(value);
        term.setType(Term.Type.and);
        terms.add(term);
        return (T) this;
    }

    public Term nest() {
        return nest(null, null);
    }

    public Term orNest() {
        return orNest(null, null);
    }

    public Term nest(String termString, Object value) {
        Term term = new Term();
        term.setColumn(termString);
        term.setValue(value);
        term.setType(Term.Type.and);
        terms.add(term);
        return term;
    }

    public Term orNest(String termString, Object value) {
        Term term = new Term();
        term.setColumn(termString);
        term.setValue(value);
        term.setType(Term.Type.or);
        terms.add(term);
        return term;
    }

    public <T extends Param> T includes(String... fields) {
        includes.addAll(Arrays.asList(fields));
        return (T) this;
    }

    public <T extends Param> T excludes(String... fields) {
        excludes.addAll(Arrays.asList(fields));
        includes.removeAll(Arrays.asList(fields));
        return (T) this;
    }

    public <T extends Param> T addTerm(Term term) {
        terms.add(term);
        return (T) this;
    }

    @Override
    @SneakyThrows
    public Param clone() {
        Param param = ((Param) super.clone());
        param.setExcludes(new LinkedHashSet<>(excludes));
        param.setIncludes(new LinkedHashSet<>(includes));
        param.setTerms(this.terms.stream().map(Term::clone).collect(Collectors.toList()));
        return param;
    }
}
