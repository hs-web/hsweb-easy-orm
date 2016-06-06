package org.hsweb.ezorm.exception;

/**
 * Created by zhouhao on 16-6-5.
 */
public class TriggerException extends RuntimeException {
    public TriggerException(String message, Throwable cause) {
        super(message, cause);
    }

    public TriggerException(String message) {
        super(message);
    }
}
