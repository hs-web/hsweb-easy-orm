package org.hswebframework.ezorm.core.dsl;

import org.hswebframework.ezorm.core.ConditionalFromBean;
import org.hswebframework.ezorm.core.NestConditionalFromBean;
import org.hswebframework.ezorm.core.TermTypeConditionalSupport;
import org.hswebframework.ezorm.core.param.QueryParam;

import java.util.List;

/**
 * @author zhouhao
 */
public final class QueryFromBean<T, Q extends QueryParam, B>
        implements ConditionalFromBean<QueryFromBean<T, Q, B>, B> {
    private Query<T, Q> proxy = null;

    public QueryFromBean(Query<T, Q> proxy) {
        this.proxy = proxy;
    }

    public Query<T, Q> fromCustom() {
        return proxy;
    }

    @Override
    public B getBean() {
        return (B) proxy.getBean();
    }

    @Override
    public NestConditionalFromBean<QueryFromBean<T, Q, B>> nest() {
        return new SimpleNestConditionalForBean<>(this, proxy.getParam().nest());
    }

    @Override
    public NestConditionalFromBean<QueryFromBean<T, Q, B>> nest(String column) {
        return new SimpleNestConditionalForBean<>(this, proxy.getParam().nest(column, getValue(column)));
    }

    @Override
    public NestConditionalFromBean<QueryFromBean<T, Q, B>> orNest() {
        return new SimpleNestConditionalForBean<>(this, proxy.getParam().orNest());
    }

    @Override
    public NestConditionalFromBean<QueryFromBean<T, Q, B>> orNest(String column) {
        return new SimpleNestConditionalForBean<>(this, proxy.getParam().orNest(column, getValue(column)));
    }

    public QueryFromBean<T, Q, B> forUpdate() {
        this.proxy.forUpdate();
        return this;
    }

    @Override
    public QueryFromBean<T, Q, B> sql(String sql, Object... params) {
        proxy.sql(sql, params);
        return this;
    }

    @Override
    public QueryFromBean<T, Q, B> and() {
        proxy.and();
        return this;
    }

    @Override
    public QueryFromBean<T, Q, B> or() {
        proxy.or();
        return this;
    }

    public QueryFromBean<T, Q, B> and(String column, String termType, Object value) {
        proxy.and(column, termType, value);
        return this;
    }

    public QueryFromBean<T, Q, B> or(String column, String termType, Object value) {
        proxy.or(column, termType, value);
        return this;
    }

    @Override
    public QueryFromBean<T, Q, B> and(String column, String termType) {
        and(column, termType, getValue(column));
        return this;
    }

    @Override
    public QueryFromBean<T, Q, B> or(String column, String termType) {
        or(column, termType, getValue(column));
        return this;
    }

    @Override
    public TermTypeConditionalSupport.Accepter<QueryFromBean<T, Q, B>, Object> getAccepter() {
        return (c, t, v) -> {
            proxy.getAccepter().accept(c, t, v);
            return this;
        };
    }

    public QueryFromBean<T, Q, B> selectExcludes(String... columns) {
        proxy.selectExcludes(columns);
        return this;
    }

    public QueryFromBean<T, Q, B> select(String... columns) {
        proxy.select(columns);
        return this;
    }

    public QueryFromBean<T, Q, B> doPaging(int pageIndex, int pageSize) {
        proxy.doPaging(pageIndex, pageSize);
        return this;
    }

    public QueryFromBean<T, Q, B> noPaging() {
        proxy.noPaging();
        return this;
    }

    public List<T> list(int pageIndex, int pageSize) {
        return proxy.list(pageIndex, pageSize);
    }

    public List<T> list(int pageIndex, int pageSize, int total) {
        return proxy.list(pageIndex, pageSize, total);
    }

    public List<T> list() {
        return proxy.list();
    }

    public List<T> listNoPaging() {
        return proxy.noPaging().list();
    }

    public <Q> List<Q> list(Query.ListExecutor<Q, QueryParam> executor) {
        return proxy.noPaging().list(executor);
    }

    public T single() {
        return proxy.single();
    }

    public int total() {
        return proxy.total();
    }

    public <Q> Q single(Query.SingleExecutor<Q, QueryParam> executor) {
        return proxy.single(executor);
    }

    public int total(Query.TotalExecutor<QueryParam> executor) {
        return proxy.total(executor);
    }

    public Q getParam() {
        return proxy.getParam();
    }

}
