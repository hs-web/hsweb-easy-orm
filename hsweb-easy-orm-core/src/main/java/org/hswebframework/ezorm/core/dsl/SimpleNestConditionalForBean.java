package org.hswebframework.ezorm.core.dsl;

import org.hswebframework.ezorm.core.NestConditionalFromBean;
import org.hswebframework.ezorm.core.SqlConditionSupport;
import org.hswebframework.ezorm.core.TermTypeConditionalFromBeanSupport;
import org.hswebframework.ezorm.core.param.SqlTerm;
import org.hswebframework.ezorm.core.param.Term;

public class SimpleNestConditionalForBean<B, T extends TermTypeConditionalFromBeanSupport<B>>
        extends SqlConditionSupport<SimpleNestConditionalForBean<B, T>>
        implements NestConditionalFromBean<B, T> {

    protected T                                               proxy;
    protected Accepter<NestConditionalFromBean<B, T>, Object> accepter = this::and;
    protected Term                                            term;

    public SimpleNestConditionalForBean(T proxy, Term term) {
        this.proxy = proxy;
        this.term = term;
    }

    @Override
    protected SimpleNestConditionalForBean<B, T> addSqlTerm(SqlTerm term) {
        this.term.addTerm(term);
        return this;
    }

    @Override
    public T end() {
        return proxy;
    }

    @Override
    public NestConditionalFromBean<B, NestConditionalFromBean<B, T>> nest() {
        return new SimpleNestConditionalForBean<>(this, term.nest());
    }

    @Override
    public NestConditionalFromBean<B, NestConditionalFromBean<B, T>> nest(String column) {
        return new SimpleNestConditionalForBean<>(this, term.nest(column, getValue(column)));
    }

    @Override
    public NestConditionalFromBean<B, NestConditionalFromBean<B, T>> orNest() {
        return new SimpleNestConditionalForBean<>(this, term.orNest());
    }

    @Override
    public NestConditionalFromBean<B, NestConditionalFromBean<B, T>> orNest(String column) {
        return new SimpleNestConditionalForBean<>(this, term.orNest(column, getValue(column)));
    }

    @Override
    public NestConditionalFromBean<B, T> and() {
        setAnd();
        accepter = this::and;
        return this;
    }

    @Override
    public NestConditionalFromBean<B, T> or() {
        setOr();
        accepter = this::or;
        return this;
    }

    @Override
    public NestConditionalFromBean<B, T> and(String column, String termType) {
        term.and(column, termType, getValue(column));
        return this;
    }

    @Override
    public NestConditionalFromBean<B, T> or(String column, String termType) {
        term.or(column, termType, getValue(column));
        return this;
    }

    public NestConditionalFromBean<B, T> and(String column, String termType, Object value) {
        term.and(column, termType, value);
        return this;
    }

    public NestConditionalFromBean<B, T> or(String column, String termType, Object value) {
        term.or(column, termType, value);
        return this;
    }

    @Override
    public Accepter<NestConditionalFromBean<B, T>, Object> getAccepter() {
        return accepter;
    }

    @Override
    public B getBean() {
        return proxy.getBean();
    }
}
