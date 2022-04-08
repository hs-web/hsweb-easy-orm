package org.hswebframework.ezorm.core;


/**
 * @author zhouhao
 */
public interface DefaultValue {
    Object get();

    static DefaultValue runtime(Object value) {
        return (RuntimeDefaultValue) () -> value;
    }
}
