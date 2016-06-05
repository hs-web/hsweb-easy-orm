package og.hsweb.ezorm.meta.expand;

import og.hsweb.ezorm.meta.TableMetaData;

/**
 * Created by zhouhao on 16-6-4.
 */
public interface ValidatorFactory {
    Validator createValidator(TableMetaData tableMetaData);
}
