package org.hsweb.ezorm.run;

/**
 * @author zhouhao
 */
public interface TermTypeConditionalSupport {
//    <Q> Q and();
//
//    <Q> Q or();
//
//    <Q> Q and(String column, Object value);
//
//    <Q> Q or(String column, Object value);
//
//    <Q> Q and(String column, String termType, Object value);
//
//    <Q> Q or(String column, String termType, Object value);
//
//    <Q> Q like(String column, Object value);
//
//    <Q> Q notLike(String column, Object value);
//
//    <Q> Q gt(String column, Object value);
//
//    <Q> Q lt(String column, Object value);
//
//    <Q> Q gte(String column, Object value);
//
//    <Q> Q lte(String column, Object value);
//
//    <Q> Q in(String column, Object value);
//
//    <Q> Q notIn(String column, Object value);
//
//    <Q> Q isEmpty(String column);
//
//    <Q> Q isNull(String column);
//
//    <Q> Q not(String column, Object value);
//
//    <Q> Q between(String column, Object between, Object and);
//
//    <Q> Q notBetween(String column, Object between, Object and);
//
//    <Q> Q accept(String column, String termType, Object value);
//
//    <Q> Accepter<Q> getAccepter();

    interface Accepter<T> {
        T accept(String column, String termType, Object value);
    }
}
