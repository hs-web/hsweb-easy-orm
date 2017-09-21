package org.hswebframework.ezorm.core;

import org.hswebframework.ezorm.core.meta.TableMetaData;

public interface ValidatorFactory {
    Validator createValidator(TableMetaData tableMetaData);
}
