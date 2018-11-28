package org.hswebframework.ezorm.core.dsl;

import org.hswebframework.ezorm.core.*;
import org.hswebframework.ezorm.core.param.UpdateParam;

import java.util.Arrays;

/**
 * @author zhouhao
 */
public final class UpdateFromBean<T, Q extends UpdateParam<T>, B>
        implements ConditionalFromBean<B, UpdateFromBean<T, Q, B>> {
    private TermTypeConditionalSupport.Accepter<UpdateFromBean<T, Q, B>, Object> accepter = this::and;
    private Update<T, Q>                                                         proxy    = null;

    public UpdateFromBean(Update<T, Q> proxy) {
        this.proxy = proxy;
    }

    @Override
    public B getBean() {
        return (B) proxy.getBean();
    }

    public Update<T, Q> fromCustom() {
        return proxy;
    }

    @Override
    public UpdateFromBean<T, Q, B> sql(String sql, Object... params) {
        proxy.sql(sql, params);
        return this;
    }

    @Override
    public NestConditionalFromBean<B, UpdateFromBean<T, Q, B>> nest() {
        return new SimpleNestConditionalForBean<>(this, proxy.getParam().nest());
    }

    @Override
    public NestConditionalFromBean<B, UpdateFromBean<T, Q, B>> nest(String column) {
        return new SimpleNestConditionalForBean<>(this, proxy.getParam().nest(column, getValue(column)));
    }

    @Override
    public NestConditionalFromBean<B, UpdateFromBean<T, Q, B>> orNest() {
        return new SimpleNestConditionalForBean<>(this, proxy.getParam().orNest());
    }

    @Override
    public NestConditionalFromBean<B, UpdateFromBean<T, Q, B>> orNest(String column) {
        return new SimpleNestConditionalForBean<>(this, proxy.getParam().orNest(column, getValue(column)));
    }

    @Override
    public UpdateFromBean<T, Q, B> and() {
        proxy.and();
        return this;
    }

    @Override
    public UpdateFromBean<T, Q, B> or() {
        proxy.or();
        return this;
    }

    public UpdateFromBean<T, Q, B> and(String column, String termType, Object value) {
        proxy.and(column, termType, value);
        return this;
    }

    public UpdateFromBean<T, Q, B> or(String column, String termType, Object value) {
        proxy.or(column, termType, value);
        return this;
    }

    @Override
    public UpdateFromBean<T, Q, B> and(String column, String termType) {
        and(column, termType, getValue(column));
        return this;
    }

    @Override
    public UpdateFromBean<T, Q, B> or(String column, String termType) {
        or(column, termType, getValue(column));
        return this;
    }

    @Override
    public TermTypeConditionalSupport.Accepter<UpdateFromBean<T, Q, B>, Object> getAccepter() {
        return accepter;
    }

    public UpdateFromBean<T, Q, B> excludes(String... columns) {
        proxy.excludes(columns);
        return this;
    }

    public UpdateFromBean<T, Q, B> includes(String... columns) {
        proxy.includes(columns);
        return this;
    }

    public UpdateFromBean<T, Q, B> includes(StaticMethodReferenceColumn... columns) {
        return includes(Arrays.stream(columns).map(StaticMethodReferenceColumn::getColumn).toArray(String[]::new));
    }

    public UpdateFromBean<T, Q, B> includes(MethodReferenceColumn... columns) {
        return includes(Arrays.stream(columns).map(MethodReferenceColumn::getColumn).toArray(String[]::new));
    }

    public UpdateFromBean<T, Q, B> excludes(StaticMethodReferenceColumn... columns) {
        return excludes(Arrays.stream(columns).map(StaticMethodReferenceColumn::getColumn).toArray(String[]::new));
    }

    public UpdateFromBean<T, Q, B> excludes(MethodReferenceColumn... columns) {
        return excludes(Arrays.stream(columns).map(MethodReferenceColumn::getColumn).toArray(String[]::new));
    }


    public int exec() {
        return proxy.exec();
    }

}
