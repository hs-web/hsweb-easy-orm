package org.hswebframework.ezorm.core.meta;

import org.hswebframework.ezorm.core.ObjectWrapperFactory;
import org.hswebframework.ezorm.core.ValidatorFactory;

public interface DatabaseMetaData {

    ObjectWrapperFactory getObjectWrapperFactory();

    ValidatorFactory getValidatorFactory();

    <T extends TableMetaData> T getTableMetaData(String name);

}
