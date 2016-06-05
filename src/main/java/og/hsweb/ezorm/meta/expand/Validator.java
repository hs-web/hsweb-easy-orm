package og.hsweb.ezorm.meta.expand;

import og.hsweb.ezorm.exception.ValidationException;

/**
 * 验证器
 * Created by zhouhao on 16-6-4.
 */
public interface Validator {
    boolean validate(Object data) throws ValidationException;
}
