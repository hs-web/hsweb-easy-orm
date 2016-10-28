package org.hsweb.ezorm.core;

import org.hsweb.ezorm.core.meta.TableMetaData;

public interface ValidatorFactory {
    Validator createValidator(TableMetaData tableMetaData);
}
