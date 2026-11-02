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

    /**
     * 将现有条件整体分组。使用独立可变列表，兼容只读输入且不修改调用方的条件列表。
     * 连续调用不会重复包裹单个分组；字段、分页等其他参数保持不变。
     *
     * @return 当前参数
     * @since 4.2.3
     */
    public <T extends Param> T group() {
        List<Term> previous = new ArrayList<>(terms);
        if (previous.size() > 1) {
            Term group = new Term();
            group.setTerms(previous);
            terms = new ArrayList<>(Collections.singletonList(group));
        } else {
            terms = previous;
        }
        return (T) this;
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
