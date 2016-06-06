package org.hsweb.ezorm.exception;

/**
 * Created by zhouhao on 16-6-5.
 */
public class ValidationException extends RuntimeException {
    private Object validateResult;

    public ValidationException(String message, Object validateResult) {
        super(message);
        this.validateResult = validateResult;
    }

    public Object getValidateResult() {
        return validateResult;
    }
}
