package org.hswebframework.ezorm.core.dsl;

import org.apache.commons.beanutils.BeanUtilsBean;
import org.apache.commons.beanutils.PropertyUtilsBean;
import org.hswebframework.ezorm.core.*;
import org.hswebframework.ezorm.core.param.SqlTerm;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.UpdateParam;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * @author zhouhao
 */
public final class Update<T, Q extends UpdateParam<T>> extends SqlConditionSupport<Update<T, Q>> implements Conditional<Update<T, Q>>, TermTypeConditionalFromBeanSupport {
    private Q param = null;
    private Accepter<Update<T, Q>, Object> accepter = this::and;
    private Executor<Q> executor;
    private static final PropertyUtilsBean propertyUtilsBean = BeanUtilsBean.getInstance().getPropertyUtils();
    private static final Logger logger = LoggerFactory.getLogger(Update.class);
    private Object bean = null;

    @Override
    public Object getBean() {
        return bean;
    }

    public Update(Q param) {
        this.param = param;
    }

    public <B> UpdateFromBean<T, Q, B> fromBean() {
        this.bean = param.getData();
        return new UpdateFromBean<>(this);
    }

    public UpdateFromBean<T, Q, T> fromBean(T bean) {
        param.setData(bean);
        this.bean = bean;
        return new UpdateFromBean<>(this);
    }

    @Override
    protected Update<T, Q> addSqlTerm(SqlTerm term) {
        return null;
    }

    public <V> Update<T, Q> set(MethodReferenceColumn<V> column) {
        return set(column.getColumn(), column.get());
    }

    public <V> Update<T, Q> set(StaticMethodReferenceColumn<V> column, Object value) {
        return set(column.getColumn(), value);
    }

    public Update<T, Q> set(String property, Object value) {
        if (param.getData() == null) {
            ((UpdateParam) param).setData(new HashMap<>());
        }
        if (param.getData() instanceof Map) {
            ((Map) param.getData()).put(property, value);
        } else {
            try {
                propertyUtilsBean.setProperty(param.getData(), property, value);
            } catch (Exception e) {
                logger.warn("set property error", e);
            }
        }
        return this;
    }

    public Update<T, Q> set(Map<String, Object> values) {
        values.forEach(this::set);
        return this;
    }

    public Update<T, Q> setExecutor(Executor<Q> executor) {
        this.executor = executor;
        return this;
    }

    public Q getParam() {
        return param;
    }

    public Update<T, Q> setParam(Q param) {
        this.param = param;
        return this;
    }

    public int exec() {
        return executor.doExecute(param);
    }

    public <R> R exec(Function<Q, R> executor) {
        return executor.apply(getParam());
    }

    public NestConditional<Update<T, Q>> nest() {
        return new SimpleNestConditional<>(this, this.param.nest());
    }

    public NestConditional<Update<T, Q>> nest(String column, Object value) {
        return new SimpleNestConditional<>(this, this.param.nest(column, value));
    }

    @Override
    public NestConditional<Update<T, Q>> orNest() {
        return new SimpleNestConditional<>(this, this.param.orNest());
    }

    @Override
    public NestConditional<Update<T, Q>> orNest(String column, Object value) {
        return new SimpleNestConditional<>(this, this.param.orNest(column, value));
    }

    @Override
    public Update<T, Q> and() {
        setAnd();
        this.accepter = this::and;
        return this;
    }

    @Override
    public Update<T, Q> or() {
        setOr();
        this.accepter = this::or;
        return this;
    }

    @Override
    public Update<T, Q> and(String column, String termType, Object value) {
        this.param.and(column, termType, value);
        return this;
    }

    @Override
    public Update<T, Q> or(String column, String termType, Object value) {
        this.param.or(column, termType, value);
        return this;
    }

    public Update<T, Q> where(String column, String termType, Object value) {
        and(column, termType, value);
        return this;
    }

    @Override
    public Accepter<Update<T, Q>, Object> getAccepter() {
        return accepter;
    }


    public Update<T, Q> excludes(String... columns) {
        param.excludes(columns);
        return this;
    }

    public Update<T, Q> includes(String... columns) {
        param.includes(columns);
        return this;
    }

    @Override
    public Update<T, Q> accept(Term term) {
        param.addTerm(term);
        return this;
    }

    @SafeVarargs
    public final <B> Update<T, Q> includes(StaticMethodReferenceColumn<B>... columns) {
        return includes(Arrays.stream(columns).map(StaticMethodReferenceColumn::getColumn).toArray(String[]::new));
    }

    @SafeVarargs
    public final <B> Update<T, Q> includes(MethodReferenceColumn<B>... columns) {
        return includes(Arrays.stream(columns).map(MethodReferenceColumn::getColumn).toArray(String[]::new));
    }

    @SafeVarargs
    public final <B> Update<T, Q> excludes(StaticMethodReferenceColumn<B>... columns) {
        return excludes(Arrays.stream(columns).map(StaticMethodReferenceColumn::getColumn).toArray(String[]::new));
    }

    @SafeVarargs
    public final <B> Update<T, Q> excludes(MethodReferenceColumn<B>... columns) {
        return excludes(Arrays.stream(columns).map(MethodReferenceColumn::getColumn).toArray(String[]::new));
    }

    @FunctionalInterface
    public interface Executor<P> {
        int doExecute(P param);
    }

    public static <T, P extends UpdateParam<T>> Update<T, P> build(Executor<P> executor, Supplier<P> paramGetter) {
        return build(executor, paramGetter.get());
    }

    public static <T, P extends UpdateParam<T>> Update<T, P> build(Executor<P> executor, P param) {
        return new Update<>(param).setExecutor(executor);
    }


    public static <T> Update<T, UpdateParam<T>> build(Executor<UpdateParam<T>> executor) {
        return build(executor, new UpdateParam());
    }
}
