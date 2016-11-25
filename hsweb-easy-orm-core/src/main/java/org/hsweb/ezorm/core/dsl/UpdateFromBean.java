package org.hsweb.ezorm.core.dsl;

import org.hsweb.ezorm.core.ConditionalFromBean;
import org.hsweb.ezorm.core.NestConditionalFromBean;
import org.hsweb.ezorm.core.TermTypeConditionalSupport;
import org.hsweb.ezorm.core.param.QueryParam;
import org.hsweb.ezorm.core.param.UpdateParam;

import java.util.List;

/**
 * @author zhouhao
 */
public final class UpdateFromBean<T, Q extends UpdateParam<T>> implements ConditionalFromBean<UpdateFromBean<T, Q>> {
    private TermTypeConditionalSupport.Accepter accepter = this::and;
    private Update<T, Q>                        proxy    = null;

    public UpdateFromBean(Update<T, Q> proxy) {
        this.proxy = proxy;
    }

    @Override
    public Object getBean() {
        return proxy.getBean();
    }

    public Update<T, Q> fromCustom() {
        return proxy;
    }

    @Override
    public NestConditionalFromBean<UpdateFromBean<T, Q>> nest() {
        return new SimpleNestConditionalForBean<>(this, proxy.getParam().nest());
    }

    @Override
    public NestConditionalFromBean<UpdateFromBean<T, Q>> nest(String column) {
        return new SimpleNestConditionalForBean<>(this, proxy.getParam().nest(column, getValue(column)));
    }

    @Override
    public NestConditionalFromBean<UpdateFromBean<T, Q>> orNest() {
        return new SimpleNestConditionalForBean<>(this, proxy.getParam().orNest());
    }

    @Override
    public NestConditionalFromBean<UpdateFromBean<T, Q>> orNest(String column) {
        return new SimpleNestConditionalForBean<>(this, proxy.getParam().orNest(column, getValue(column)));
    }

    @Override
    public UpdateFromBean<T, Q> and() {
        accepter = this::and;
        return this;
    }

    @Override
    public UpdateFromBean<T, Q> or() {
        accepter = this::or;
        return this;
    }

    public UpdateFromBean<T, Q> and(String column, String termType, Object value) {
        and();
        proxy.and(column, termType, value);
        return this;
    }

    public UpdateFromBean<T, Q> or(String column, String termType, Object value) {
        or();
        proxy.or(column, termType, value);
        return this;
    }

    @Override
    public UpdateFromBean<T, Q> and(String column, String termType) {
        and(column, termType, getValue(column));
        return this;
    }

    @Override
    public UpdateFromBean<T, Q> or(String column, String termType) {
        or(column, termType, getValue(column));
        return this;
    }

    @Override
    public TermTypeConditionalSupport.Accepter<UpdateFromBean<T, Q>> getAccepter() {
        return accepter;
    }

    public UpdateFromBean<T, Q> excludes(String... columns) {
        proxy.excludes(columns);
        return this;
    }

    public UpdateFromBean<T, Q> includes(String... columns) {
        proxy.includes(columns);
        return this;
    }

    public int exec() {
        return proxy.exec();
    }

}
