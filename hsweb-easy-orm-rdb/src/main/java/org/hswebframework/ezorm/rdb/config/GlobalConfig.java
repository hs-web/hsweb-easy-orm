package org.hswebframework.ezorm.rdb.config;

import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.rdb.utils.ApacheCommonPropertyOperator;

public class GlobalConfig {

    private static ObjectPropertyOperator propertyOperator = ApacheCommonPropertyOperator.INSTANCE;

    public static ObjectPropertyOperator getPropertyOperator() {
        return propertyOperator;
    }

    public static void setPropertyOperator(ObjectPropertyOperator propertyOperator) {
        GlobalConfig.propertyOperator = propertyOperator;
    }
}
