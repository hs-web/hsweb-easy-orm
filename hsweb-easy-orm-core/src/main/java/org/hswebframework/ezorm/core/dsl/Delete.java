package org.hswebframework.ezorm.core.dsl;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.NestConditional;
import org.hswebframework.ezorm.core.SimpleNestConditional;
import org.hswebframework.ezorm.core.param.Param;
import org.hswebframework.ezorm.core.param.Term;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * @author zhouhao
 */
public final class Delete<P extends Param> implements Conditional<Delete<P>> {
    private P param;
    private Accepter<Delete<P>, Object> accepter = this::and;

    public Delete(P param) {
        this.param = param;
    }

    public P getParam() {
        return param;
    }

    public Delete<P> setParam(P param) {
        this.param = param;
        return this;
    }

    @Override
    public Delete<P> accept(Term term) {
        param.addTerm(term);
        return this;
    }

    public NestConditional<Delete<P>> nest() {
        return new SimpleNestConditional<>(this, this.param.nest());
    }

    @Override
    public NestConditional<Delete<P>> orNest() {
        return new SimpleNestConditional<>(this, this.param.orNest());
    }


    @Override
    public Delete<P> and() {
        this.accepter = this::and;
        return this;
    }

    @Override
    public Delete<P> or() {
        this.accepter = this::or;
        return this;
    }

    @Override
    public Delete<P> and(String column, String termType, Object value) {
        if (value == null) {
            return this;
        }
        this.param.and(column, termType, value);
        return this;
    }

    @Override
    public Delete<P> or(String column, String termType, Object value) {
        if (value == null) {
            return this;
        }
        this.param.or(column, termType, value);
        return this;
    }

    public Delete<P> where(String column, String termType, Object value) {
        if (value == null) {
            return this;
        }
        and(column, termType, value);
        return this;
    }

    @Override
    public Accepter<Delete<P>, Object> getAccepter() {
        return accepter;
    }

    public <R> R execute(Function<P, R> executor) {
        return executor.apply(getParam());
    }

    public static Delete<Param> of() {
        return new Delete<>(new Param());
    }

    public static <P extends Param> Delete<P> of(Supplier<P> supplier) {
        return new Delete<>(supplier.get());
    }
}
