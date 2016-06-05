package og.hsweb.ezorm.run.simple;

import og.hsweb.ezorm.meta.TableMetaData;
import og.hsweb.ezorm.meta.expand.Validator;

import java.util.Map;

/**
 * Created by zhouhao on 16-6-5.
 */
public abstract class ValidatorAndTriggerSupport {

    void tryValidate(Object date) {
        Validator validator = getTableMeta().getValidator();
        if (validator != null) {
            validator.validate(date);
        }
    }

    void trigger(String name, Map<String, Object> root) {
        TableMetaData metaData = getTableMeta();
        metaData.on(name, root);
    }

    abstract TableMetaData getTableMeta();
}
