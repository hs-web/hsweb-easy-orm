package org.hsweb.ezorm.core.meta;

import org.hsweb.ezorm.core.ObjectWrapperFactory;
import org.hsweb.ezorm.core.ValidatorFactory;

public interface DatabaseMetaData {

    ObjectWrapperFactory getObjectWrapperFactory();

    ValidatorFactory getValidatorFactory();

    <T extends TableMetaData> T getTableMetaData(String name);

}
