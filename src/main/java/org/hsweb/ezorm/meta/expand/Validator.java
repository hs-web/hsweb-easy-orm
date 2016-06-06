package org.hsweb.ezorm.meta.expand;

import org.hsweb.ezorm.exception.ValidationException;

/**
 * 验证器
 * Created by zhouhao on 16-6-4.
 */
public interface Validator {
    boolean validate(Object data, Operation operation) throws ValidationException;

    enum Operation {
        INSERT, UPDATE
    }
}
