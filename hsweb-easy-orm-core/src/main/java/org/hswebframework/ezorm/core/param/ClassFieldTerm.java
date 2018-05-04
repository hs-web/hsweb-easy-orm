package org.hswebframework.ezorm.core.param;

import java.lang.reflect.Field;

/**
 * @author zhouhao
 * @since 3.0
 */
public class ClassFieldTerm extends Term {
    private Field field;

    public void setField(Field field) {
        this.field = field;
    }

    public Field getField() {
        return field;
    }
}
