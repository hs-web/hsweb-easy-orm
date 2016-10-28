package org.hsweb.ezorm.rdb.run.simple;

import org.hsweb.ezorm.core.NestConditional;
import org.hsweb.ezorm.core.TermTypeConditionalSupport;
import org.hsweb.ezorm.core.param.Term;

public class SimpleNestConditional<T extends TermTypeConditionalSupport> implements NestConditional<T> {
    Term term;
    T    target;
    Accepter<NestConditional<T>> accepter = this::and;

    public SimpleNestConditional(T target, Term term) {
        this.term = term;
        this.target = target;
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
    public Accepter<NestConditional<T>> getAccepter() {
        return accepter;
    }

    @Override
    public NestConditional<NestConditional<T>> nest() {
        return new SimpleNestConditional(this, this.term.nest());
    }

    @Override
    public NestConditional<NestConditional<T>> nest(String column, Object value) {
        return new SimpleNestConditional(this, this.term.nest(column, value));
    }

    @Override
    public NestConditional<NestConditional<T>> orNest() {
        return new SimpleNestConditional(this, this.term.orNest());
    }

    @Override
    public NestConditional<NestConditional<T>> orNest(String column, Object value) {
        return new SimpleNestConditional(this, this.term.orNest(column, value));
    }

    @Override
    public NestConditional<T> and(String column, String termType, Object value) {
        and();
        term.and(column, termType, value);
        return this;
    }

    @Override
    public NestConditional<T> or(String column, String termType, Object value) {
        or();
        term.or(column, termType, value);
        return this;
    }
}
