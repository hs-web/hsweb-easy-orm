package org.hswebframework.ezorm.core.dsl;

import org.hswebframework.ezorm.core.*;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.UpdateParam;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

/**
 * @author zhouhao
 */
public final class Update<T, Q extends UpdateParam<T>> implements Conditional<Update<T, Q>> {
    private Q param = null;
    private Accepter<Update<T, Q>, Object> accepter = this::and;

    private Update(Q param) {
        this.param = param;
    }

    public <V> Update<T, Q> set(MethodReferenceColumn<V> column) {
        return set(column.getColumn(), column.get());
    }

    public <V> Update<T, Q> set(StaticMethodReferenceColumn<V> column, Object value) {
        return set(column.getColumn(), value);
    }

    public Update<T, Q> set(String property, Object value) {

        GlobalConfig.getPropertyOperator().setProperty(param.getData(), property, value);

        return this;
    }

    public Update<T, Q> set(Map<String, Object> values) {
        values.forEach(this::set);
        return this;
    }

    public Q getParam() {
        return param;
    }

    public Update<T, Q> setParam(Q param) {
        this.param = param;
        return this;
    }

    public <R> R execute(Function<Q, R> executor) {
        return executor.apply(getParam());
    }

    public NestConditional<Update<T, Q>> nest() {
        return new SimpleNestConditional<>(this, this.param.nest());
    }

    @Override
    public NestConditional<Update<T, Q>> orNest() {
        return new SimpleNestConditional<>(this, this.param.orNest());
    }

    @Override
    public Update<T, Q> and() {
        this.accepter = this::and;
        return this;
    }

    @Override
    public Update<T, Q> or() {
        this.accepter = this::or;
        return this;
    }

    @Override
    public Update<T, Q> and(String column, String termType, Object value) {
        if (value == null) {
            return this;
        }
        this.param.and(column, termType, value);
        return this;
    }

    @Override
    public Update<T, Q> or(String column, String termType, Object value) {
        if (value == null) {
            return this;
        }
        this.param.or(column, termType, value);
        return this;
    }

    public Update<T, Q> where(String column, String termType, Object value) {
        if (value == null) {
            return this;
        }
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

    public static Update<Map<String, Object>, UpdateParam<Map<String, Object>>> of() {
        return of(new HashMap<>());
    }

    public static <T> Update<T, UpdateParam<T>> of(T param) {
        Objects.requireNonNull(param, "param");
        return of(new UpdateParam<>(param));
    }

    public static <T, Q extends UpdateParam<T>> Update<T, Q> of(Q param) {
        Objects.requireNonNull(param, "param");
        Objects.requireNonNull(param.getData(), "param.data");
        return new Update<>(param);
    }

}
