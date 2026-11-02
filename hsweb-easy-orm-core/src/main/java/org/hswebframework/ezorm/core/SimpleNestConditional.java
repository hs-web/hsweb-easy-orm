package org.hswebframework.ezorm.core;

import org.hswebframework.ezorm.core.param.Param;
import org.hswebframework.ezorm.core.param.Term;

/**
 * 基于单个 Term 构造嵌套条件，保留承载 Term 与父级构造器的身份。
 * 分组仅重组当前层子条件；实例可变，不保证并发修改安全。
 *
 * @param <T> 父级构造器类型
 * @see NestConditional
 */
public class SimpleNestConditional<T extends TermTypeConditionalSupport>
        implements NestConditional<T> {
    private final Term term;
    private final T target;
    private Accepter<NestConditional<T>, Object> accepter = this::and;

    public SimpleNestConditional(T target, Term term) {
        this.term = term;
        this.target = target;
    }

    @Override
    public NestConditional<T> group() {
        Param grouped = new Param();
        grouped.setTerms(term.getTerms());
        grouped.group();
        term.setTerms(grouped.getTerms());
        return this;
    }

    @Override
    public NestConditional<T> accept(Term term) {
        this.term.addTerm(term);
        return this;
    }

    @Override
    public T end() {
        return target;
    }


    @Override
    public NestConditional<T> and() {
        accepter = this::and;
        return this;
    }

    @Override
    public NestConditional<T> or() {
        accepter = this::or;
        return this;
    }

    @Override
    public Accepter<NestConditional<T>, Object> getAccepter() {
        return accepter;
    }

    @Override
    public NestConditional<NestConditional<T>> nest() {
        return new SimpleNestConditional<>(this, this.term.nest());
    }

    @Override
    public NestConditional<NestConditional<T>> nest(String column, Object value) {
        return new SimpleNestConditional<>(this, this.term.nest(column, value));
    }

    @Override
    public NestConditional<NestConditional<T>> orNest() {
        return new SimpleNestConditional<>(this, this.term.orNest());
    }

    @Override
    public NestConditional<NestConditional<T>> orNest(String column, Object value) {
        return new SimpleNestConditional<>(this, this.term.orNest(column, value));
    }

    @Override
    public NestConditional<T> and(String column, String termType, Object value) {
        term.and(column, termType, value);
        return this;
    }

    @Override
    public NestConditional<T> or(String column, String termType, Object value) {
        term.or(column, termType, value);
        return this;
    }
}
