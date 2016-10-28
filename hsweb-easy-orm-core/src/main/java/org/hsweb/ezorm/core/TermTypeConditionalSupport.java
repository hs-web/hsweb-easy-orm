package org.hsweb.ezorm.core;

public interface TermTypeConditionalSupport {
    interface Accepter<T> {
        T accept(String column, String termType, Object value);
    }
}
