package org.hsweb.ezorm.run;

import org.hsweb.ezorm.param.TermType;

import java.util.Arrays;

public interface NestConditional<T extends TermTypeConditionalSupport> extends TermTypeConditionalSupport {

    T end();

    NestConditional<NestConditional<T>> nest();

    NestConditional<NestConditional<T>> nest(String column, Object value);

    NestConditional<NestConditional<T>> orNest();

    NestConditional<NestConditional<T>> orNest(String column, Object value);

    default NestConditional<T> and(String column, Object value) {
        and();
        return and(column, TermType.eq, value);
    }

    default NestConditional<T> or(String column, Object value) {
        or();
        return or(column, TermType.eq, value);
    }

    NestConditional<T> and();

    NestConditional<T> or();

    NestConditional<T> and(String column, String termType, Object value);

    NestConditional<T> or(String column, String termType, Object value);

    default NestConditional<T> like(String column, Object value) {
        return accept(column, TermType.like, value);
    }

    default NestConditional<T> notLike(String column, Object value) {
        return accept(column, TermType.nlike, value);
    }

    default NestConditional<T> gt(String column, Object value) {
        return accept(column, TermType.gt, value);
    }

    default NestConditional<T> lt(String column, Object value) {
        return accept(column, TermType.lt, value);
    }

    default NestConditional<T> gte(String column, Object value) {
        return accept(column, TermType.gte, value);
    }

    default NestConditional<T> lte(String column, Object value) {
        return accept(column, TermType.lte, value);
    }

    default NestConditional<T> in(String column, Object value) {
        return accept(column, TermType.in, value);
    }

    default NestConditional<T> notIn(String column, Object value) {
        return accept(column, TermType.nin, value);
    }

    default NestConditional<T> isEmpty(String column) {
        return accept(column, TermType.empty, 1);
    }

    default NestConditional<T> isNull(String column) {
        return accept(column, TermType.isnull, 1);
    }

    default NestConditional<T> not(String column, Object value) {
        return accept(column, TermType.not, value);
    }

    default NestConditional<T> between(String column, Object between, Object and) {
        return accept(column, TermType.btw, Arrays.asList(between, and));
    }

    default NestConditional<T> notBetween(String column, Object between, Object and) {
        return accept(column, TermType.nbtw, Arrays.asList(between, and));
    }

    default NestConditional<T> accept(String column, String termType, Object value) {
        return getAccepter().accept(column, termType, value);
    }

    Accepter<NestConditional<T>> getAccepter();
}
