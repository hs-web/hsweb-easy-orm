package org.hsweb.ezorm.core.dsl;

import org.hsweb.ezorm.core.*;
import org.hsweb.ezorm.core.param.QueryParam;
import org.hsweb.ezorm.core.param.SqlTerm;

import java.util.List;
import java.util.function.Supplier;

/**
 * 查询条件构造器,用于构造{@link QueryParam} 以及设置执行器进行执行
 *
 * @author zhouhao
 * @see Conditional
 * @see SqlConditionSupport
 * @since 1.1
 */
public final class Query<T, Q extends QueryParam> extends SqlConditionSupport<Query<T, Q>> implements Conditional<Query<T, Q>>, TermTypeConditionalFromBeanSupport {
    private Q                    param          = null;
    private Accepter<Query<T, Q>,Object>             accepter       = this::and;
    private ListExecutor<T, Q>   listExecutor   = null;
    private TotalExecutor<Q>     totalExecutor  = null;
    private SingleExecutor<T, Q> singleExecutor = null;
    private Object               bean           = null;

    public Query(Q param) {
        this.param = param;
    }

    public Query<T, Q> setListExecutor(ListExecutor<T, Q> listExecutor) {
        this.listExecutor = listExecutor;
        return this;
    }

    public Query<T, Q> setTotalExecutor(TotalExecutor<Q> totalExecutor) {
        this.totalExecutor = totalExecutor;
        return this;
    }

    public Query<T, Q> setSingleExecutor(SingleExecutor<T, Q> singleExecutor) {
        this.singleExecutor = singleExecutor;
        return this;
    }

    @Override
    public Object getBean() {
        return bean;
    }

    public QueryFromBean<T, Q> fromBean(Object bean) {
        this.bean = bean;
        return new QueryFromBean<>(this);
    }

    @Override
    protected Query<T, Q> addSqlTerm(SqlTerm term) {
        param.addTerm(term);
        return this;
    }

    public Q getParam() {
        return param;
    }

    public Query<T, Q> setParam(Q param) {
        this.param = param;
        return this;
    }

    public Query<T, Q> orderByAsc(String column) {
        param.orderBy(column).asc();
        return this;
    }

    public Query<T, Q> orderByDesc(String column) {
        param.orderBy(column).desc();
        return this;
    }

    public Query<T, Q> doPaging(int pageIndex, int pageSize) {
        param.doPaging(pageIndex, pageSize);
        return this;
    }

    public Query<T, Q> noPaging() {
        param.setPaging(false);
        return this;
    }

    public List<T> list(int pageIndex, int pageSize) {
        doPaging(pageIndex, pageSize);
        return listExecutor.doExecute(param);
    }

    public List<T> list(int pageIndex, int pageSize, int total) {
        doPaging(pageIndex, pageSize);
        param.rePaging(total);
        return listExecutor.doExecute(param);
    }

    public List<T> list() {
        return listExecutor.doExecute(param);
    }


    public List<T> listNoPaging() {
        return noPaging().list();
    }

    public <Q> List<Q> list(ListExecutor<Q, QueryParam> executor) {
        return executor.doExecute(param);
    }

    public T single() {
        return singleExecutor.doExecute(param);
    }

    public int total() {
        return totalExecutor.doExecute(param);
    }

    public <Q> Q single(SingleExecutor<Q, QueryParam> executor) {
        return executor.doExecute(param);
    }

    public int total(TotalExecutor<QueryParam> executor) {
        return executor.doExecute(param);
    }

    public NestConditional<Query<T, Q>> nest() {
        return new SimpleNestConditional(this, this.param.nest());
    }

    public NestConditional<Query<T, Q>> nest(String column, Object value) {
        return new SimpleNestConditional(this, this.param.nest(column, value));
    }

    @Override
    public NestConditional<Query<T, Q>> orNest() {
        return new SimpleNestConditional(this, this.param.orNest());
    }

    @Override
    public NestConditional<Query<T, Q>> orNest(String column, Object value) {
        return new SimpleNestConditional(this, this.param.orNest(column, value));
    }

    @Override
    public Query<T, Q> and() {
        setAnd();
        this.accepter = this::and;
        return this;
    }

    @Override
    public Query<T, Q> or() {
        setOr();
        this.accepter = this::or;
        return this;
    }

    @Override
    public Query<T, Q> and(String column, String termType, Object value) {
        this.param.and(column, termType, value);
        return this;
    }

    @Override
    public Query<T, Q> or(String column, String termType, Object value) {
        this.param.or(column, termType, value);
        return this;
    }

    public Query<T, Q> where(String column, String termType, Object value) {
        and(column, termType, value);
        return this;
    }

    @Override
    public Accepter<Query<T, Q>,Object> getAccepter() {
        return accepter;
    }

    @FunctionalInterface
    public interface ListExecutor<R, P extends QueryParam> {
        List<R> doExecute(P param);
    }


    @FunctionalInterface
    public interface SingleExecutor<R, P extends QueryParam> {
        R doExecute(P param);
    }

    @FunctionalInterface
    public interface TotalExecutor<P extends QueryParam> {
        int doExecute(P param);
    }

    public Query<T, Q> selectExcludes(String... columns) {
        param.excludes(columns);
        return this;
    }

    public Query<T, Q> select(String... columns) {
        param.includes(columns);
        return this;
    }

    public static <R, P extends QueryParam> Query<R, P> forList(ListExecutor<R, P> executor, Supplier<P> paramGetter) {
        return forList(executor, paramGetter.get());
    }

    public static <R, P extends QueryParam> Query<R, P> forList(ListExecutor<R, P> executor, P param) {
        return new Query<R, P>(param).setListExecutor(executor);
    }

    public static <R> Query<R, QueryParam> forList(ListExecutor<R, QueryParam> executor) {
        return forList(executor, new QueryParam());
    }

    public static <R, P extends QueryParam> Query<R, P> forSingle(SingleExecutor<R, P> executor, Supplier<P> paramGetter) {
        return forSingle(executor, paramGetter.get());
    }

    public static <R, P extends QueryParam> Query<R, P> forSingle(SingleExecutor<R, P> executor, P param) {
        return new Query<R, P>(param).setSingleExecutor(executor);
    }

    public static <R> Query<R, QueryParam> forSingle(SingleExecutor<R, QueryParam> executor) {
        return forSingle(executor, new QueryParam());
    }

    public static <R, P extends QueryParam> Query<R, P> forTotal(TotalExecutor<P> executor, Supplier<P> paramGetter) {
        return forTotal(executor, paramGetter.get());
    }

    public static <R, P extends QueryParam> Query<R, P> forTotal(TotalExecutor<P> executor, P param) {
        return new Query<R, P>(param).setTotalExecutor(executor);
    }

    public static <R, P extends QueryParam> Query<R, P> empty(P param) {
        return new Query<>(param);
    }

    public static <R> Query<R, QueryParam> forTotal(TotalExecutor<QueryParam> executor) {
        return forTotal(executor, new QueryParam());
    }
}
