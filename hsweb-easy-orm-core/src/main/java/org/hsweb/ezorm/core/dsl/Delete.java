package org.hsweb.ezorm.core.dsl;

import org.hsweb.ezorm.core.Conditional;
import org.hsweb.ezorm.core.NestConditional;
import org.hsweb.ezorm.core.SimpleNestConditional;
import org.hsweb.ezorm.core.param.Param;
import org.hsweb.ezorm.core.param.Param;

/**
 * @author zhouhao
 */
public final class Delete implements Conditional<Delete> {
    private Param    param    = null;
    private Accepter accepter = this::and;
    private Executor<Param> executor;

    public Delete() {
        this(new Param());
    }


    public Delete(Param param) {
        this.param = param;
    }

    public Delete setExecutor(Executor<Param> executor) {
        this.executor = executor;
        return this;
    }

    public Param getParam() {
        return param;
    }

    public Delete setParam(Param param) {
        this.param = param;
        return this;
    }

    public int exec() {
        return executor.doExecute(param);
    }

    public NestConditional<Delete> nest() {
        return new SimpleNestConditional(this, this.param.nest());
    }

    public NestConditional<Delete> nest(String column, Object value) {
        return new SimpleNestConditional(this, this.param.nest(column, value));
    }

    @Override
    public NestConditional<Delete> orNest() {
        return new SimpleNestConditional(this, this.param.orNest());
    }

    @Override
    public NestConditional<Delete> orNest(String column, Object value) {
        return new SimpleNestConditional(this, this.param.orNest(column, value));
    }


    @Override
    public Delete and() {
        this.accepter = this::and;
        return this;
    }

    @Override
    public Delete or() {
        this.accepter = this::or;
        return this;
    }

    @Override
    public Delete and(String column, String termType, Object value) {
        this.param.and(column, termType, value);
        return this;
    }

    @Override
    public Delete or(String column, String termType, Object value) {
        this.param.or(column, termType, value);
        return this;
    }

    public Delete where(String column, String termType, Object value) {
        and(column, termType, value);
        return this;
    }

    @Override
    public Accepter<Delete> getAccepter() {
        return accepter;
    }

    @FunctionalInterface
    public interface Executor<P> {
        int doExecute(P param);
    }

    public Delete excludes(String... columns) {
        param.excludes(columns);
        return this;
    }

    public Delete includes(String... columns) {
        param.includes(columns);
        return this;
    }
}
