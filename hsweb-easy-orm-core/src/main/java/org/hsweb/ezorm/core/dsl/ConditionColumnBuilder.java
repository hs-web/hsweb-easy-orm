package org.hsweb.ezorm.core.dsl;

import org.hsweb.ezorm.core.Conditional;

import java.util.Collection;
import java.util.function.Consumer;

/**
 * TODO 完成注释
 *
 * @author zhouhao
 */
public class ConditionColumnBuilder {
    private final String column;

    private ConditionColumnBuilder(String column) {
        this.column = column;
    }

    public static ConditionColumnBuilder build(String column) {
        return new ConditionColumnBuilder(column);
    }

    public <T extends Conditional> Consumer<T> accept(String termType, Object value) {
        return (conditional -> conditional.accept(column, termType, value));
    }

    public <T extends Conditional> Consumer<T> is(Object value) {
        return (conditional -> conditional.is(column, value));
    }

    public <T extends Conditional> Consumer<T> like(Object value) {
        return (conditional -> conditional.like(column, value));
    }

    public <T extends Conditional> Consumer<T> like$(Object value) {
        return (conditional -> conditional.like$(column, value));
    }

    public <T extends Conditional> Consumer<T> $like(Object value) {
        return (conditional -> conditional.$like(column, value));
    }

    public <T extends Conditional> Consumer<T> $like$(Object value) {
        return (conditional -> conditional.$like$(column, value));
    }

    public <T extends Conditional> Consumer<T> notLike(Object value) {
        return (conditional -> conditional.notLike(column, value));
    }

    public <T extends Conditional> Consumer<T> gt(Object value) {
        return (conditional -> conditional.gt(column, value));
    }

    public <T extends Conditional> Consumer<T> lt(Object value) {
        return (conditional -> conditional.lt(column, value));
    }

    public <T extends Conditional> Consumer<T> gte(Object value) {
        return (conditional -> conditional.gte(column, value));
    }

    public <T extends Conditional> Consumer<T> lte(Object value) {
        return (conditional -> conditional.lte(column, value));
    }

    public <T extends Conditional> Consumer<T> in(Object value) {
        return (conditional -> conditional.in(column, value));
    }

    public <T extends Conditional> Consumer<T> in(Object... values) {
        return (conditional -> conditional.in(column, values));
    }

    public <T extends Conditional> Consumer<T> in(Collection values) {
        return (conditional -> conditional.in(column, values));
    }

    public <T extends Conditional> Consumer<T> notIn(Object value) {
        return (conditional -> conditional.notIn(column, value));
    }

    public <T extends Conditional> Consumer<T> isEmpty() {
        return (conditional -> conditional.isEmpty(column));
    }

    public <T extends Conditional> Consumer<T> notEmpty() {
        return (conditional -> conditional.notEmpty(column));
    }

    public <T extends Conditional> Consumer<T> isNull() {
        return (conditional -> conditional.isNull(column));
    }

    public <T extends Conditional> Consumer<T> notNull() {
        return (conditional -> conditional.notNull(column));
    }

    public <T extends Conditional> Consumer<T> not(Object value) {
        return (conditional -> conditional.not(column, value));
    }

    public <T extends Conditional> Consumer<T> between(Object between, Object and) {
        return (conditional -> conditional.between(column, between, and));
    }

    public <T extends Conditional> Consumer<T> notBetween(Object between, Object and) {
        return (conditional -> conditional.between(column, between, and));
    }
}
